const API_BASE_URL = window.RTF_API_URL || 'http://localhost:8000';
const NICKNAMES_URL = 'data/nicknames.json';
console.info(`[RTF] API_BASE=${API_BASE_URL}`);
const DEFAULT_N_SIMS = 10000;
const DEFAULT_SEED = 4242;
const RTF_BUILD_ID = 'frontend-appjs-' + new Date().toISOString();
console.info('[RTF] app.js build', RTF_BUILD_ID);

// State
let simulationMode = 'this-week'; // 'historical', 'this-week', 'hypothetical'
let selectedPlayer = null;
let selectedTeam = null;
let selectedDate = null;
let selectedSeason = null;
let selectedWeek = null;
let selectedGame = null;
let nextGame = null;
let homeAway = 'vs'; // 'vs' or 'away'
let pendingRunId = null;

let players = [];
let teams = [];
let playerGames = [];
let availableSeasons = [];
const teamNameMap = new Map();
const teamSlugMap = new Map();
const opponentCache = new Map();
let currentOpponentSet = null;
let nicknameMap = new Map();
let nicknameReverseMap = new Map();
let simulationInFlight = false;
let simulationRunCounter = 0;
let terminalQueue = [];
let terminalFlushActive = false;
let terminalRunToken = 0;
let terminalRng = null;

function buildApiUrl(path) {
    if (!path) return API_BASE_URL;
    if (/^https?:\/\//i.test(path)) return path;
    if (path.startsWith('/')) return `${API_BASE_URL}${path}`;
    return `${API_BASE_URL}/${path}`;
}

async function fetchJson(path, options = {}) {
    const url = buildApiUrl(path);
    const response = await fetch(url, options);
    const DEBUG_API = (window.RTF_DEBUG === true) || (String(window.RTF_DEBUG) === '1');
    let data = null;
    let rawText = '';
    try {
        rawText = await response.text();
        if (DEBUG_API) {
            console.debug('[RTF] fetchJson response', { url, status: response.status });
            console.debug('[RTF] fetchJson raw text', { url, rawText: rawText.slice(0, 300) });
        }
        data = rawText ? JSON.parse(rawText) : null;
        if (DEBUG_API) {
            console.debug('[RTF] fetchJson parsed JSON keys', {
                url,
                keys: data && typeof data === 'object' ? Object.keys(data) : null
            });
        }
    } catch (err) {
        data = null;
    }
    if (!response.ok) {
        const message = data && data.message ? data.message : `Request failed (${response.status}).`;
        const serialized = data ? JSON.stringify(data) : null;
        console.error('[RTF] API error', { url, status: response.status, errorCode: data && data.error_code, message, data: serialized || data });
        throw new Error(message);
    }
    if (data && data.ok === false) {
        const message = data.message || 'Request failed.';
        const errorCode = data.error_code || 'unknown_error';
        const serialized = data ? JSON.stringify(data) : null;
        console.error('[RTF] API error', { url, status: response.status, errorCode, message, data: serialized || data });
        throw new Error(message);
    }
    // Canonical API envelope unwrapping
    let payload = data;
    if (data && typeof data === 'object') {
        if (Object.prototype.hasOwnProperty.call(data, 'result')) payload = data.result;
        else if (Object.prototype.hasOwnProperty.call(data, 'data')) payload = data.data;
    }
    if (DEBUG_API) {
        console.debug('[RTF] fetchJson payload keys', {
            url,
            keys: payload && typeof payload === 'object' ? Object.keys(payload) : null
        });
    }
    return payload;
}

function normalizeSearchToken(value) {
    if (!value) return '';
    return value
        .toString()
        .toLowerCase()
        .replace(/[^a-z0-9\s]/g, ' ')
        .replace(/\s+/g, ' ')
        .trim();
}

function getSeedOverride() {
    const override = window.RTF_SEED;
    if (override === null || typeof override === 'undefined') return null;
    const parsed = Number(override);
    return Number.isFinite(parsed) ? parsed : null;
}

function generateSeed() {
    if (window.crypto && window.crypto.getRandomValues) {
        const buf = new Uint32Array(1);
        window.crypto.getRandomValues(buf);
        return buf[0];
    }
    return Number(Date.now() % 4294967295);
}

const TERMINAL_PHASES = [
    'initialization',
    'context_build',
    'model_setup',
    'monte_carlo',
    'aggregation',
    'finalize'
];

const TERMINAL_TEMPLATES = {
    initialization: [
        'Initializing simulation context for {player_name}...',
        'Booting simulation pipeline for {player_name} ({position})...',
        'Preparing simulation session for {player_name}...'
    ],
    context_build: [
        'Loading recent usage windows for {player_name}...',
        'Aggregating historical context for {player_name}...',
        'Evaluating {position} trends and efficiency signals...'
    ],
    model_setup: [
        'Resolving opponent profile ({opponent})...',
        'Combining matchup context with {position} priors...',
        'Configuring model inputs for {mode} scenario...'
    ],
    monte_carlo: [
        'Running Monte Carlo (n={n_sims})... seed={seed}',
        'Sampling outcome space across {n_sims} games... seed={seed}',
        'Generating distribution draws for {player_name}... seed={seed}'
    ],
    aggregation: [
        'Aggregating simulation draws into percentiles...',
        'Computing upside/downside ranges and medians...',
        'Summarizing expected outcomes for {player_name}...'
    ],
    finalize: [
        'Finalizing report artifacts...',
        'Preparing distributions and summary tables...',
        'Simulation complete. Proceed to results \u2193'
    ]
};

function makeSeededRng(seed) {
    let t = (Number(seed) || 0) >>> 0;
    return function rng() {
        t += 0x6D2B79F5;
        let r = Math.imul(t ^ (t >>> 15), 1 | t);
        r ^= r + Math.imul(r ^ (r >>> 7), 61 | r);
        return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
    };
}

function interpolateTemplate(template, context) {
    return template.replace(/\{(\w+)\}/g, (_, key) => {
        const value = context && Object.prototype.hasOwnProperty.call(context, key) ? context[key] : '';
        return value == null ? '' : String(value);
    });
}

function pickTemplate(phase, rng) {
    const list = TERMINAL_TEMPLATES[phase] || [];
    if (list.length === 0) return '';
    const idx = Math.floor(rng() * list.length);
    return list[Math.min(idx, list.length - 1)];
}

function buildTerminalContext(payload) {
    const playerName = payload && payload.player_name ? payload.player_name : 'Unknown Player';
    const position = payload && payload.position ? payload.position : (selectedPlayer ? selectedPlayer.position : '');
    const opponent = payload && payload.schedule_opponent
        ? getTeamName(payload.schedule_opponent)
        : (selectedTeam ? getTeamName(selectedTeam) : '--');
    const mode = payload && payload.mode ? payload.mode : (simulationMode || '');
    return {
        player_name: playerName,
        position: normalizePosition(position) || '',
        opponent: opponent || '--',
        n_sims: payload && payload.n_sims ? payload.n_sims : DEFAULT_N_SIMS,
        seed: payload && payload.seed ? payload.seed : DEFAULT_SEED,
        mode: mode || ''
    };
}

function appendTerminalLine(text, className = 'info', prefix = '') {
    const terminalOutput = document.getElementById('terminalOutput');
    if (!terminalOutput) return;
    const line = document.createElement('div');
    line.className = `terminal-line ${className}`;
    line.textContent = `${prefix}${text}`;
    const cursorWrapper = terminalOutput.querySelector('.terminal-line-wrapper');
    if (cursorWrapper && cursorWrapper.parentNode === terminalOutput) {
        terminalOutput.insertBefore(line, cursorWrapper);
    } else {
        terminalOutput.appendChild(line);
    }
    terminalOutput.scrollTop = terminalOutput.scrollHeight;
}

function clearTerminal() {
    const terminalOutput = document.getElementById('terminalOutput');
    if (!terminalOutput) return;
    terminalOutput.innerHTML = '<div class="terminal-line-wrapper"><span class="terminal-cursor" id="terminalCursor">_</span></div>';
}

function emitPhaseMessages(phase, rng, context, allowSecond = false) {
    const template = pickTemplate(phase, rng);
    if (template) {
        enqueueTerminalLine(interpolateTemplate(template, context), 'info');
    }
    if (allowSecond && rng && (TERMINAL_TEMPLATES[phase] || []).length > 1) {
        const template2 = pickTemplate(phase, rng);
        if (template2 && template2 !== template) {
            enqueueTerminalLine(interpolateTemplate(template2, context), 'info');
        }
    }
}

function enqueueTerminalLine(text, className = 'info', prefix = '') {
    terminalQueue.push({ text: text || '', className, prefix });
}

function getTerminalRng() {
    return terminalRng || (() => 0.5);
}

function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function typeText(element, text, runToken) {
    if (!text) return;
    const rng = getTerminalRng();
    const charDelay = 4 + Math.floor(rng() * 7);
    for (let i = 0; i < text.length; i++) {
        if (runToken !== terminalRunToken) return;
        element.textContent += text[i];
        await delay(charDelay);
    }
}

async function renderTerminalLine(line, runToken) {
    const terminalOutput = document.getElementById('terminalOutput');
    if (!terminalOutput || runToken !== terminalRunToken) return;
    const rng = getTerminalRng();
    const lineElement = document.createElement('div');
    lineElement.className = `terminal-line ${line.className || 'info'}`;
    lineElement.textContent = '';
    const cursorWrapper = terminalOutput.querySelector('.terminal-line-wrapper');
    if (cursorWrapper && cursorWrapper.parentNode === terminalOutput) {
        terminalOutput.insertBefore(lineElement, cursorWrapper);
    } else {
        terminalOutput.appendChild(lineElement);
    }
    terminalOutput.scrollTop = terminalOutput.scrollHeight;
    const lineDelay = 40 + Math.floor(rng() * 50);
    await delay(lineDelay);
    const prefix = line.prefix || '';
    if (prefix) {
        lineElement.textContent = prefix;
    }
    await typeText(lineElement, line.text || '', runToken);
    terminalOutput.scrollTop = terminalOutput.scrollHeight;
}

async function flushTerminalQueue(runToken) {
    if (terminalFlushActive) return;
    terminalFlushActive = true;
    while (terminalQueue.length > 0 && runToken === terminalRunToken) {
        const line = terminalQueue.shift();
        await renderTerminalLine(line, runToken);
    }
    terminalFlushActive = false;
}

function scrollToTerminal() {
    const terminalSection = document.getElementById('rtf-terminal-section');
    if (!terminalSection) return;
    terminalSection.scrollIntoView({ behavior: 'smooth', block: 'start' });
}

function normalizePosition(pos) {
    const DEBUG_API = (window.RTF_DEBUG === true) || (String(window.RTF_DEBUG) === '1');
    if (pos == null) return '';
    if (typeof pos === 'string') return pos.trim().toUpperCase();
    if (Array.isArray(pos) && pos.length > 0) {
        if (DEBUG_API) console.debug('[RTF] normalizePosition non-string array', { value: pos });
        return String(pos[0]).trim().toUpperCase();
    }
    if (typeof pos === 'object') {
        if (typeof pos.position === 'string') return pos.position.trim().toUpperCase();
        if (typeof pos.code === 'string') return pos.code.trim().toUpperCase();
        if (DEBUG_API) {
            console.debug('[RTF] normalizePosition non-string object', {
                keys: Object.keys(pos),
                value: pos
            });
        }
    } else if (DEBUG_API) {
        console.debug('[RTF] normalizePosition non-string value', { type: typeof pos, value: pos });
    }
    return String(pos).trim().toUpperCase();
}

function buildNicknameMaps(payload) {
    nicknameMap = new Map();
    nicknameReverseMap = new Map();
    if (!payload || typeof payload !== 'object') return;

    Object.entries(payload).forEach(([nickname, names]) => {
        const key = normalizeSearchToken(nickname);
        if (!key || !Array.isArray(names)) return;
        nicknameMap.set(key, names);
        names.forEach((name) => {
            const nameKey = normalizeSearchToken(name);
            if (!nameKey) return;
            if (!nicknameReverseMap.has(nameKey)) {
                nicknameReverseMap.set(nameKey, new Set());
            }
            nicknameReverseMap.get(nameKey).add(key);
        });
    });
}

async function loadNicknames() {
    try {
        const response = await fetch(NICKNAMES_URL, { cache: 'no-store' });
        if (!response.ok) {
            throw new Error(`Nickname map missing (${response.status})`);
        }
        const payload = await response.json();
        buildNicknameMaps(payload);
    } catch (err) {
        console.warn('[RTF] Nickname map unavailable', err);
    }
}

function buildPlayerSearchIndex() {
    players.forEach((player) => {
        const tokens = new Set();
        tokens.add(normalizeSearchToken(player.player_name));
        tokens.add(normalizeSearchToken(player.position));
        tokens.add(normalizeSearchToken(player.team));
        const nameKey = normalizeSearchToken(player.player_name);
        if (nicknameReverseMap.has(nameKey)) {
            nicknameReverseMap.get(nameKey).forEach((nick) => tokens.add(nick));
        }
        player.search_tokens = Array.from(tokens).filter(Boolean);
    });
}

function buildTeamNameMap() {
    teamNameMap.clear();
    teamSlugMap.clear();
    teams.forEach(team => {
        if (team.team) {
            teamNameMap.set(team.team, team.name || team.team);
            if (team.slug) {
                teamSlugMap.set(team.team, team.slug);
            }
        }
    });
}

function getTeamName(teamId) {
    if (!teamId) return 'Unknown Team';
    return teamNameMap.get(teamId) || teamId;
}

function getTeamSlug(teamId) {
    if (!teamId) return '';
    return teamSlugMap.get(teamId) || '';
}

function getPlayerInitials(name) {
    if (!name) return '';
    const parts = name.split(' ').filter(Boolean);
    if (parts.length === 1) return parts[0].slice(0, 2).toUpperCase();
    return `${parts[0][0]}${parts[parts.length - 1][0]}`.toUpperCase();
}

async function loadInitialData() {
    try {
        const DEBUG_API = (window.RTF_DEBUG === true) || (String(window.RTF_DEBUG) === '1');
        console.info('[RTF] loadInitialData start', RTF_BUILD_ID);
        await loadNicknames();
        const [playersResp, teamsResp, seasonsResp] = await Promise.all([
            fetchJson('/players'),
            fetchJson('/teams'),
            fetchJson('/seasons')
        ]);
        const playersPayload = playersResp;
        const teamsPayload = teamsResp;
        const seasonsPayload = seasonsResp;
        players = Array.isArray(playersPayload)
            ? playersPayload
            : (playersPayload?.players ?? []);
        teams = Array.isArray(teamsPayload)
            ? teamsPayload
            : (teamsPayload?.teams ?? []);
        availableSeasons = Array.isArray(seasonsPayload)
            ? seasonsPayload
            : (seasonsPayload?.seasons ?? []);
        if (!Array.isArray(players) || players.length === 0) {
            console.error('[RTF] Player load failed', {
                endpoint: '/players',
                rawResponseType: typeof playersResp,
                rawKeys: playersResp && typeof playersResp === 'object'
                    ? Object.keys(playersResp)
                    : null,
                rawSample: JSON.stringify(playersResp).slice(0, 400)
            });
            throw new Error('[RTF] Player payload invalid');
        }
        if (!Array.isArray(teams) || teams.length === 0) {
            console.error('[RTF] Team load failed', {
                endpoint: '/teams',
                rawResponseType: typeof teamsResp,
                rawKeys: teamsResp && typeof teamsResp === 'object'
                    ? Object.keys(teamsResp)
                    : null,
                rawSample: JSON.stringify(teamsResp).slice(0, 400)
            });
            throw new Error('[RTF] Team payload invalid');
        }
        if (!Array.isArray(availableSeasons) || availableSeasons.length === 0) {
            console.error('[RTF] Seasons load failed', {
                endpoint: '/seasons',
                rawResponseType: typeof seasonsResp,
                rawKeys: seasonsResp && typeof seasonsResp === 'object'
                    ? Object.keys(seasonsResp)
                    : null,
                rawSample: JSON.stringify(seasonsResp).slice(0, 400)
            });
            throw new Error('[RTF] Seasons payload invalid');
        }
        if (DEBUG_API) {
            console.info('[RTF] First player sample', players[0]);
        }
        buildTeamNameMap();
        buildPlayerSearchIndex();
        if (!players[0] || !Array.isArray(players[0].search_tokens) || players[0].search_tokens.length === 0) {
            const sample = players[0] || null;
            const reason = !sample
                ? 'missing player sample'
                : `missing search tokens (player_name=${sample.player_name}, team=${sample.team}, position=${sample.position})`;
            throw new Error(`[RTF] Search index build failed: ${reason}`);
        }
        if (DEBUG_API) {
            console.debug('[RTF] verify players length', players.length, 'teams length', teams.length);
        }
        populateSeasonWeekSelects();
        initializeTeamSelection();
        console.info(`[RTF] Loaded players=${players.length}, teams=${teams.length}`);
    } catch (err) {
        const searchInput = document.getElementById('player-search');
        if (searchInput) {
            searchInput.placeholder = 'Unable to load players';
        }
        console.error(err);
    }
}

function populateSeasonWeekSelects() {
    const seasonSelect = document.getElementById('season-select');
    const weekSelect = document.getElementById('week-select');
    if (!seasonSelect || !weekSelect) return;

    seasonSelect.innerHTML = '<option value="">-- SELECT SEASON --</option>';
    weekSelect.innerHTML = '<option value="">-- SELECT WEEK --</option>';

    const seasons = availableSeasons.length > 0 ? availableSeasons : [];
    seasons.forEach((season) => {
        const opt = document.createElement('option');
        opt.value = season;
        opt.textContent = season;
        seasonSelect.appendChild(opt);
    });

    for (let wk = 1; wk <= 18; wk++) {
        const opt = document.createElement('option');
        opt.value = wk;
        opt.textContent = `Week ${wk}`;
        weekSelect.appendChild(opt);
    }

    seasonSelect.addEventListener('change', (e) => {
        selectedSeason = e.target.value ? parseInt(e.target.value, 10) : null;
        updateCalculateButton();
    });
    weekSelect.addEventListener('change', (e) => {
        selectedWeek = e.target.value ? parseInt(e.target.value, 10) : null;
        updateCalculateButton();
    });
}

// Initialize on DOM load
document.addEventListener('DOMContentLoaded', () => {
    console.info('[RTF] DOMContentLoaded', RTF_BUILD_ID);
    initializeModeToggle();
    initializePlayerSearch();
    initializeHomeAwayToggle();
    initializeDateSelection();
    initializeSimulationButton();
    const params = new URLSearchParams(window.location.search);
    pendingRunId = params.get('run_id');
    loadInitialData().then(() => {
        switchMode('this-week');
        updateCalculateButton();
        if (pendingRunId) {
            loadRunFromStorage(pendingRunId);
        }
    });
});


// Player Search Functionality
function initializePlayerSearch() {
    const searchInput = document.getElementById('player-search');
    const autocompleteResults = document.getElementById('autocomplete-results');
    const selectedPlayerDiv = document.getElementById('selected-player');

    if (!searchInput || !autocompleteResults) return;

    searchInput.addEventListener('input', (e) => {
        const query = e.target.value.trim().toLowerCase();
        
        if (query.length === 0) {
            autocompleteResults.classList.remove('active');
            return;
        }

        // Only show offensive skilled positions: QB, RB, WR, TE, K
        const allowedPositions = ['QB', 'RB', 'WR', 'TE', 'K'];
        const nicknameMatches = nicknameMap.get(normalizeSearchToken(query)) || [];
        const nicknameMatchSet = new Set(nicknameMatches.map(name => normalizeSearchToken(name)));
        const filtered = players.filter(player => {
            if (!allowedPositions.includes(player.position)) return false;
            const tokens = Array.isArray(player.search_tokens) ? player.search_tokens : [];
            if (tokens.some(token => token.includes(query))) return true;
            if (nicknameMatchSet.size > 0) {
                return nicknameMatchSet.has(normalizeSearchToken(player.player_name));
            }
            return false;
        });

        displayAutocompleteResults(filtered);
    });

    // Close autocomplete when clicking outside
    document.addEventListener('click', (e) => {
        if (!searchInput.contains(e.target) && !autocompleteResults.contains(e.target)) {
            autocompleteResults.classList.remove('active');
        }
    });
}

function displayAutocompleteResults(players) {
    const autocompleteResults = document.getElementById('autocomplete-results');
    if (!autocompleteResults) return;

    if (players.length === 0) {
        autocompleteResults.classList.remove('active');
        return;
    }

    autocompleteResults.innerHTML = players.map(player => {
        const teamSlug = getTeamSlug(player.team);
        const teamClass = teamSlug ? `team-${teamSlug}` : '';
        return `
        <div class="autocomplete-item" data-player-id="${player.player_id}">
            <div class="player-headshot">${getPlayerInitials(player.player_name)}</div>
            <div class="player-info">
                <div class="player-name">${player.player_name}</div>
                <div class="player-details">${player.position} • <span class="team-badge ${teamClass}">${player.team || 'N/A'}</span>${player.eligible === false ? ' • Ineligible' : ''}</div>
            </div>
        </div>
    `;
    }).join('');

    autocompleteResults.classList.add('active');

    // Add click handlers
    autocompleteResults.querySelectorAll('.autocomplete-item').forEach(item => {
        item.addEventListener('click', () => {
            const playerId = item.dataset.playerId;
            selectPlayer(playerId);
        });
    });
}

function selectPlayer(playerId) {
    const player = players.find(p => p.player_id === playerId);
    if (!player) return;

    selectedPlayer = player;
    selectedTeam = null;
    selectedDate = null;
    selectedGame = null;
    nextGame = null;
    playerGames = [];
    currentOpponentSet = opponentCache.get(player.player_id) || null;
    
    const searchInput = document.getElementById('player-search');
    const autocompleteResults = document.getElementById('autocomplete-results');
    const selectedPlayerDiv = document.getElementById('selected-player');

    if (searchInput) searchInput.value = player.player_name;
    if (autocompleteResults) autocompleteResults.classList.remove('active');
    
    if (selectedPlayerDiv) {
        const teamSlug = getTeamSlug(player.team);
        const teamClass = teamSlug ? `team-${teamSlug}` : '';
        selectedPlayerDiv.innerHTML = `
            <div style="display: flex; align-items: center; gap: 0.75rem;">
                <div class="player-headshot">${getPlayerInitials(player.player_name)}</div>
                <div style="flex: 1; min-width: 0;">
                    <div class="player-name">${player.player_name}</div>
                    <div class="player-details">
                        ${player.position} • <span class="team-badge ${teamClass}">${player.team || 'N/A'}</span>${player.eligible === false ? ' • Ineligible' : ''}
                    </div>
                </div>
            </div>
        `;
        selectedPlayerDiv.classList.add('active');
    }
    
    // Mode-specific updates
    if (simulationMode === 'this-week') {
        updateThisWeekMatchup();
    }
    loadPlayerGames();
    
    updateCalculateButton();
}

// Mode Toggle Functionality
function initializeModeToggle() {
    const modeButtons = document.querySelectorAll('.mode-toggle-btn');
    
    modeButtons.forEach(btn => {
        btn.addEventListener('click', () => {
            const mode = btn.dataset.mode;
            switchMode(mode);
        });
    });
}

function switchMode(mode) {
    simulationMode = mode;
    
    // Update toggle buttons
    document.querySelectorAll('.mode-toggle-btn').forEach(btn => {
        btn.classList.remove('active');
    });
    const modeButton = document.getElementById(`mode-${mode}`);
    if (modeButton) {
        modeButton.classList.add('active');
    }
    
    // Update subtitle
    const subtitle = document.getElementById('mode-subtitle');
    const helperText = document.getElementById('mode-helper-text');
    
    switch(mode) {
        case 'historical':
            if (subtitle) subtitle.textContent = 'Re-simulate a past matchup using historical context.';
            if (helperText) helperText.innerHTML = '<p>Re-simulate a past matchup using historical context.</p>';
            break;
        case 'this-week':
            if (subtitle) subtitle.textContent = 'Project performance for this week\'s scheduled matchup.';
            if (helperText) helperText.innerHTML = '<p>Opponent and location are locked to this week\'s schedule.</p>';
            break;
        case 'hypothetical':
            if (subtitle) subtitle.textContent = 'Simulate a matchup that is not scheduled.';
            if (helperText) helperText.innerHTML = '<p>Uses the player\'s current form and the opponent\'s current state.</p>';
            break;
    }
    
    // Show/hide input sections
    document.querySelectorAll('.mode-inputs').forEach(section => {
        section.style.display = 'none';
    });
    
    const activeInputs = document.getElementById(`inputs-${mode}`);
    if (activeInputs) {
        activeInputs.style.display = 'flex';
    }
    
    // Reset incompatible inputs
    if (mode !== 'historical') {
        selectedDate = null;
        selectedGame = null;
        const dateSelect = document.getElementById('game-date-select');
        if (dateSelect) {
            dateSelect.value = '';
            dateSelect.innerHTML = '<option value="">-- SELECT DATE --</option>';
        }
    }
    
    if (mode !== 'hypothetical') {
        // Reset home/away for non-hypothetical modes
        homeAway = 'vs';
        const toggleVs = document.getElementById('toggle-vs');
        const toggleAway = document.getElementById('toggle-away');
        if (toggleVs) toggleVs.classList.add('active');
        if (toggleAway) toggleAway.classList.remove('active');
    }
    
    if (mode === 'this-week') {
        updateThisWeekMatchup();
    } else {
        // Clear auto-filled fields
        const autoOpponent = document.getElementById('auto-opponent');
        const autoLocation = document.getElementById('auto-location');
        if (autoOpponent) autoOpponent.textContent = '--';
        if (autoLocation) autoLocation.textContent = '--';
        nextGame = null;
    }
    
    // Re-initialize team selection for the active mode
    initializeTeamSelection();
    
    // If switching to historical mode and player/team already selected, populate dates
    if (mode === 'historical' && selectedPlayer) {
        loadPlayerGames();
    }
    
    // Update button state
    updateCalculateButton();
}

async function updateThisWeekMatchup() {
    if (!selectedPlayer) return;

    const autoOpponent = document.getElementById('auto-opponent');
    const autoLocation = document.getElementById('auto-location');
    if (autoOpponent) autoOpponent.textContent = 'Loading...';
    if (autoLocation) autoLocation.textContent = '--';

    try {
        const data = await fetchJson(`/player/${selectedPlayer.player_id}/next_game`);
        nextGame = data.next_game || null;
        if (!nextGame) {
            const fallbackMessage = data.reason === 'season_complete' ? 'Season complete.' : 'No upcoming game found.';
            const message = data.message || fallbackMessage;
            if (autoOpponent) autoOpponent.textContent = message;
            if (autoLocation) autoLocation.textContent = '--';
            selectedSeason = null;
            selectedWeek = null;
            selectedTeam = null;
            updateCalculateButton();
            return;
        }
        selectedSeason = nextGame.season;
        selectedWeek = nextGame.week;
        selectedTeam = nextGame.opponent;
        homeAway = nextGame.home_away === 'AWAY' ? 'away' : 'vs';

        const opponentName = getTeamName(nextGame.opponent);
        if (autoOpponent) autoOpponent.textContent = opponentName;
        if (autoLocation) autoLocation.textContent = nextGame.home_away === 'AWAY' ? '@ (AWAY)' : 'VS (HOME)';

        const seasonSelect = document.getElementById('season-select');
        const weekSelect = document.getElementById('week-select');
        if (seasonSelect) seasonSelect.value = nextGame.season;
        if (weekSelect) weekSelect.value = nextGame.week;
    } catch (err) {
        nextGame = null;
        selectedSeason = null;
        selectedWeek = null;
        selectedTeam = null;
        if (autoOpponent) autoOpponent.textContent = err.message || 'No upcoming game found for this player';
        if (autoLocation) autoLocation.textContent = '--';
    }

    updateCalculateButton();
}

// Team Selection Functionality
function initializeTeamSelection() {
    let teamSelectionDiv;
    if (simulationMode === 'historical') {
        teamSelectionDiv = document.getElementById('team-selection-historical');
    } else if (simulationMode === 'hypothetical') {
        teamSelectionDiv = document.getElementById('team-selection-hypothetical');
    } else {
        return;
    }

    if (!teamSelectionDiv) return;

    let teamList = teams;
    if (!teamList || teamList.length === 0) {
        teamList = buildFallbackTeamsFromOpponents();
    }

    renderTeamSelection(teamSelectionDiv, teamList);
}

function renderTeamSelection(container, teamList) {
    if (!container) return;

    const grouped = groupTeamsByConference(teamList);
    let html = '';
    grouped.forEach(group => {
        html += '<div class="division-group">';
        html += `<div class="division-title">${group.label}</div>`;
        html += '<div class="team-grid">';
        group.teams.forEach(team => {
            const teamClass = team.slug ? `team-${team.slug}` : '';
            const teamId = team.team || team.abbr || team.id;
            if (!teamId) return;
            const disabled = shouldDisableTeam(teamId);
            const disabledStyle = disabled ? 'opacity:0.35; filter:grayscale(1); cursor:not-allowed;' : '';
            const tooltip = disabled
                ? 'This player has never faced this team. Try a hypothetical matchup.'
                : '';
            html += `
                <div class="team-item ${teamClass}" data-team-id="${teamId}" data-disabled="${disabled ? 'true' : 'false'}" style="${disabledStyle}" title="${tooltip}">
                    <span class="team-name">${team.name || teamId}</span>
                </div>
            `;
        });
        html += '</div></div>';
    });
    container.innerHTML = html;

    container.querySelectorAll('.team-item').forEach(item => {
        item.addEventListener('click', () => {
            const teamId = item.dataset.teamId;
            if (item.dataset.disabled === 'true') {
                return;
            }
            selectTeam(teamId);
        });
    });

    if (selectedTeam) {
        const selectedItem = container.querySelector(`[data-team-id="${selectedTeam}"]`);
        if (selectedItem) {
            selectedItem.classList.add('selected');
        }
    }
}

function getOpponentTeams() {
    if (!playerGames || playerGames.length === 0) return [];
    const opponents = new Set(playerGames.map(game => game.opponent).filter(Boolean));
    if (teams.length === 0) {
        return Array.from(opponents).map(teamId => ({ team: teamId, name: teamId }));
    }
    return teams.filter(team => opponents.has(team.team));
}

function buildFallbackTeamsFromOpponents() {
    const opponents = currentOpponentSet ? Array.from(currentOpponentSet) : [];
    return opponents.map(teamId => ({ team: teamId, name: teamId }));
}

function shouldDisableTeam(teamId) {
    if (!selectedPlayer) return false;
    if (simulationMode === 'hypothetical') return false;
    if (!currentOpponentSet || currentOpponentSet.size === 0) return true;
    return !currentOpponentSet.has(teamId);
}

function groupTeamsByConference(teamList) {
    const hasConference = teamList.some(team => team.conference && team.division);
    if (!hasConference) {
        return [{ label: 'Teams', teams: teamList }];
    }
    const conferenceOrder = ['AFC', 'NFC'];
    const divisionOrder = ['East', 'North', 'South', 'West'];
    const groups = [];
    conferenceOrder.forEach(conf => {
        divisionOrder.forEach(div => {
            const filtered = teamList.filter(team => {
                const teamConf = (team.conference || '').toUpperCase();
                const teamDiv = (team.division || '').toUpperCase();
                return teamConf === conf && teamDiv === div.toUpperCase();
            });
            if (filtered.length > 0) {
                groups.push({
                    label: `${conf} ${div}`,
                    teams: filtered
                });
            }
        });
    });
    return groups.length > 0 ? groups : [{ label: 'Teams', teams: teamList }];
}

function selectTeam(teamId) {
    // Remove previous selection (only in the active mode's container)
    let activeContainer;
    if (simulationMode === 'historical') {
        activeContainer = document.getElementById('team-selection-historical');
    } else if (simulationMode === 'hypothetical') {
        activeContainer = document.getElementById('team-selection-hypothetical');
    }
    
    if (activeContainer) {
        activeContainer.querySelectorAll('.team-item').forEach(item => {
            item.classList.remove('selected');
        });
    }

    // Add selection to clicked team
    const selectedItem = activeContainer ? activeContainer.querySelector(`[data-team-id="${teamId}"]`) : null;
    if (selectedItem) {
        selectedItem.classList.add('selected');
        selectedTeam = teamId;
        
        // Update historical dates if in historical mode
        if (simulationMode === 'historical' && selectedPlayer) {
            updateHistoricalDates();
        }
        
        updateCalculateButton();
    }
}

// Date Selection Functionality
function initializeDateSelection() {
    const dateSelect = document.getElementById('game-date-select');
    if (!dateSelect) return;
    
    dateSelect.addEventListener('change', (e) => {
        const option = e.target.selectedOptions[0];
        selectedDate = e.target.value;
        if (option) {
            selectedGame = {
                season: option.dataset.season ? parseInt(option.dataset.season, 10) : null,
                week: option.dataset.week ? parseInt(option.dataset.week, 10) : null,
                opponent: option.dataset.opponent || null,
                home_away: option.dataset.homeAway || null,
                game_date: option.dataset.gameDate || null
            };
            selectedSeason = selectedGame.season;
            selectedWeek = selectedGame.week;

            const seasonSelect = document.getElementById('season-select');
            const weekSelect = document.getElementById('week-select');
            if (seasonSelect && selectedSeason) seasonSelect.value = selectedSeason;
            if (weekSelect && selectedWeek) weekSelect.value = selectedWeek;
        } else {
            selectedGame = null;
        }
        updateCalculateButton();
    });
}

async function loadPlayerGames() {
    if (!selectedPlayer) return;
    try {
        const data = await fetchJson(`/player/${selectedPlayer.player_id}/games`);
        playerGames = data.games || [];
        const opponents = new Set(playerGames.map(game => game.opponent).filter(Boolean));
        opponentCache.set(selectedPlayer.player_id, opponents);
        currentOpponentSet = opponents;
        initializeTeamSelection();
        if (selectedTeam && !playerGames.some(game => game.opponent === selectedTeam)) {
            selectedTeam = null;
        }
        updateHistoricalDates();
    } catch (err) {
        playerGames = [];
        currentOpponentSet = opponentCache.get(selectedPlayer.player_id) || null;
        selectedTeam = null;
        updateHistoricalDates();
    }
}

function updateHistoricalDates() {
    const dateSelect = document.getElementById('game-date-select');
    if (!dateSelect) return;

    if (!selectedPlayer || !selectedTeam || playerGames.length === 0) {
        dateSelect.innerHTML = '<option value="">-- SELECT DATE --</option>';
        dateSelect.value = '';
        selectedDate = null;
        selectedGame = null;
        return;
    }

    const games = playerGames.filter(game => game.opponent === selectedTeam);
    if (games.length === 0) {
        dateSelect.innerHTML = '<option value="">-- NO GAMES FOUND --</option>';
        dateSelect.value = '';
        selectedDate = null;
        selectedGame = null;
        return;
    }

    dateSelect.innerHTML = '';
    games.forEach((game, idx) => {
        const option = document.createElement('option');
        option.value = `${game.season}-wk${game.week}-${game.opponent}`;
        option.dataset.season = game.season;
        option.dataset.week = game.week;
        option.dataset.opponent = game.opponent;
        option.dataset.homeAway = game.home_away;
        option.dataset.gameDate = game.game_date;

        const formattedDate = formatGameDateLabel(game.game_date);
        const opponentName = getTeamName(game.opponent);
        const location = game.home_away === 'AWAY' ? '@' : 'vs';
        option.textContent = formattedDate
            ? `${game.season} Week ${game.week} ${location} ${opponentName} (${formattedDate})`
            : `${game.season} Week ${game.week} ${location} ${opponentName} (Unknown date)`;
        dateSelect.appendChild(option);
        if (idx == 0) {
            dateSelect.value = option.value;
            selectedDate = option.value;
            selectedGame = {
                season: option.dataset.season ? parseInt(option.dataset.season, 10) : null,
                week: option.dataset.week ? parseInt(option.dataset.week, 10) : null,
                opponent: option.dataset.opponent || null,
                home_away: option.dataset.homeAway || null,
                game_date: option.dataset.gameDate || null
            };
            selectedSeason = selectedGame.season;
            selectedWeek = selectedGame.week;
        }
    });
    updateCalculateButton();
}

function formatGameDateLabel(rawDate) {
    if (rawDate == null || rawDate === '' || rawDate === 0 || rawDate === '0') return null;
    if (typeof rawDate === 'string') {
        const trimmed = rawDate.trim();
        if (!trimmed) return null;
        if (/^\d{4}-\d{2}-\d{2}$/.test(trimmed)) return trimmed;
        const parsed = new Date(trimmed);
        if (!Number.isNaN(parsed.getTime())) {
            return parsed.toLocaleDateString('en-US', { year: 'numeric', month: 'short', day: 'numeric' });
        }
        return trimmed;
    }
    const parsed = new Date(rawDate);
    if (!Number.isNaN(parsed.getTime())) {
        return parsed.toLocaleDateString('en-US', { year: 'numeric', month: 'short', day: 'numeric' });
    }
    return null;
}

// Home/Away Toggle Functionality
function initializeHomeAwayToggle() {
    const toggleVs = document.getElementById('toggle-vs');
    const toggleAway = document.getElementById('toggle-away');
    
    if (!toggleVs || !toggleAway) return;

    toggleVs.addEventListener('click', () => {
        homeAway = 'vs';
        toggleVs.classList.add('active');
        toggleAway.classList.remove('active');
        updateCalculateButton();
    });

    toggleAway.addEventListener('click', () => {
        homeAway = 'away';
        toggleAway.classList.add('active');
        toggleVs.classList.remove('active');
        updateCalculateButton();
    });
}

function resolvePayload() {
    if (!selectedPlayer) {
        return { valid: false, message: 'Select a player.' };
    }
    if (selectedPlayer.eligible === false) {
        return { valid: false, message: selectedPlayer.eligibility_reason || 'Player is ineligible for simulation.' };
    }

    let season = selectedSeason;
    let week = selectedWeek;
    let availabilityPolicy = 'played_only';
    let mode = 'historical_replay';
    let scheduleOpponent = null;
    let scheduleHomeAway = null;

    if (simulationMode === 'historical') {
        if (!selectedGame || !selectedGame.season || !selectedGame.week) {
            return { valid: false, message: 'Select a valid historical game to continue.' };
        }
        season = selectedGame.season;
        week = selectedGame.week;
        availabilityPolicy = 'played_only';
        mode = 'historical_replay';
        scheduleOpponent = selectedGame.opponent || null;
        scheduleHomeAway = selectedGame.home_away || null;
    } else if (simulationMode === 'this-week') {
        if (!nextGame || !nextGame.season || !nextGame.week) {
            return { valid: false, message: 'No upcoming game found for this player.' };
        }
        season = nextGame.season;
        week = nextGame.week;
        availabilityPolicy = 'expected_active';
        mode = 'upcoming_game';
        scheduleOpponent = nextGame.opponent || null;
        scheduleHomeAway = nextGame.home_away || null;
    } else if (simulationMode === 'hypothetical') {
        if (!selectedTeam) {
            return { valid: false, message: 'Select an opponent.' };
        }
        availabilityPolicy = 'force_counterfactual';
        mode = 'hypothetical_matchup';
        scheduleOpponent = selectedTeam;
        scheduleHomeAway = homeAway === 'away' ? 'AWAY' : 'HOME';
    }

    if (simulationMode === 'hypothetical') {
        if (!selectedSeason && !selectedWeek) {
            // Let backend resolve latest season/week from player history.
            season = null;
            week = null;
        }
    }

    const seedOverride = getSeedOverride();
    const resolvedSeed = seedOverride != null ? seedOverride : generateSeed();
    return {
        valid: true,
        payload: {
            player_id: selectedPlayer.player_id || null,
            player_name: selectedPlayer.player_name || null,
            season: season,
            week: week,
            n_sims: DEFAULT_N_SIMS,
            seed: resolvedSeed,
            availability_policy: availabilityPolicy,
            schema_version: 'v1',
            mode: mode,
            schedule_opponent: scheduleOpponent,
            schedule_home_away: scheduleHomeAway
        }
    };
}

function findSummaryValue(summary, names) {
    if (!Array.isArray(summary)) return null;
    const list = Array.isArray(names) ? names : [names];
    for (const name of list) {
        const row = summary.find(item => item.stat === name);
        if (row && typeof row.p50 !== 'undefined') {
            return row.p50;
        }
    }
    return null;
}

function buildRunStats(position, summary) {
    const pos = normalizePosition(position);
    const stats = {};
    stats.fantasy_points = findSummaryValue(summary, ['fantasy_points', 'fantasy_ppr', 'ppr_points']);

    if (pos === 'QB') {
        stats.passing_yards = findSummaryValue(summary, 'passing_yards');
        stats.passing_tds = findSummaryValue(summary, 'passing_tds');
        stats.rush_yards = findSummaryValue(summary, ['qb_rush_yards', 'rushing_yards']);
        stats.rush_tds = findSummaryValue(summary, ['qb_rush_tds', 'rushing_tds']);
        stats.pass_attempts = findSummaryValue(summary, ['passing_attempts', 'target_pass_attempts_qb']);
        stats.completions = findSummaryValue(summary, ['passing_completions', 'target_completions_qb']);
        stats.rush_attempts = findSummaryValue(summary, ['qb_rush_attempts', 'rush_attempts', 'carries']);
    } else if (pos === 'RB' || pos === 'WR' || pos === 'TE') {
        stats.rushing_yards = findSummaryValue(summary, 'rushing_yards');
        stats.receiving_yards = findSummaryValue(summary, 'receiving_yards');
        stats.total_touchdowns = findSummaryValue(summary, ['total_touchdowns']);
        stats.targets = findSummaryValue(summary, 'targets');
        stats.receptions = findSummaryValue(summary, 'receptions');
        stats.carries = findSummaryValue(summary, ['rush_attempts', 'carries']);
    } else if (pos === 'K') {
        stats.fg_made = findSummaryValue(summary, ['fg_made', 'field_goals_made']);
        stats.fg_attempts = findSummaryValue(summary, ['fg_attempts', 'field_goal_attempts']);
        stats.xp_made = findSummaryValue(summary, ['pat_made', 'extra_points_made']);
        stats.xp_attempts = findSummaryValue(summary, ['pat_attempts', 'extra_point_attempts']);
    }
    return stats;
}

function normalizeStatKey(value) {
    return String(value || '').toLowerCase().replace(/[^a-z0-9]+/g, '');
}

function getSummaryTables(report) {
    if (!report) return [];
    if (Array.isArray(report.summary_tables)) return report.summary_tables;
    if (Array.isArray(report.summary)) return report.summary;
    return [];
}

function getRowValue(row, key) {
    if (!row || typeof row !== 'object') return null;
    const target = normalizeStatKey(key);
    for (const [k, v] of Object.entries(row)) {
        if (normalizeStatKey(k) === target) return v;
    }
    return null;
}

function getStatRow(summaryTables, names) {
    const list = Array.isArray(names) ? names : [names];
    const candidates = list.map(name => normalizeStatKey(name));
    return summaryTables.find(row => candidates.includes(normalizeStatKey(row.stat || row.name)));
}

function formatStatValue(value) {
    if (value == null || Number.isNaN(value)) return '—';
    if (typeof value !== 'number') return String(value);
    const rounded = Math.round(value * 10) / 10;
    return Number.isInteger(rounded) ? String(rounded) : rounded.toFixed(1);
}

function orderSummaryRows(rows, position) {
    const pos = normalizePosition(position);
    const orderSpec = [];
    if (pos === 'QB') {
        orderSpec.push(
            ['passing_completions', 'completions'],
            ['passing_attempts', 'attempts'],
            ['passing_yards'],
            ['qb_rush_attempts', 'rushing_attempts', 'rush_attempts'],
            ['qb_rush_yards', 'rushing_yards'],
            ['interceptions_thrown', 'interceptions'],
            ['qb_sacks_taken', 'sacks_taken'],
            ['passing_tds', 'pass_tds'],
            ['qb_rush_tds'],
            ['fantasy_ppr', 'fantasy_points', 'ppr_points']
        );
    } else if (pos === 'RB') {
        orderSpec.push(
            ['targets'],
            ['receptions'],
            ['rushing_attempts', 'carries', 'rush_attempts'],
            ['rushing_yards'],
            ['receiving_yards'],
            ['total_touchdowns'],
            ['fantasy_ppr', 'fantasy_points', 'ppr_points']
        );
    } else if (pos === 'WR' || pos === 'TE') {
        orderSpec.push(
            ['targets'],
            ['receptions'],
            ['receiving_yards'],
            ['total_touchdowns'],
            ['fantasy_ppr', 'fantasy_points', 'ppr_points']
        );
    } else if (pos === 'K') {
        orderSpec.push(
            ['fg_made', 'field_goals_made'],
            ['fg_attempts', 'field_goal_attempts'],
            ['xp_made', 'pat_made', 'extra_points_made'],
            ['xp_attempts', 'pat_attempts', 'extra_point_attempts'],
            ['fantasy_ppr', 'fantasy_points', 'ppr_points']
        );
    }

    const remaining = rows.slice();
    const ordered = [];
    orderSpec.forEach((aliases) => {
        const aliasKeys = aliases.map(normalizeStatKey);
        const idx = remaining.findIndex((row) => aliasKeys.includes(normalizeStatKey(row.stat || row.name)));
        if (idx >= 0) {
            ordered.push(remaining.splice(idx, 1)[0]);
        }
    });

    const tail = remaining.sort((a, b) => String(a.stat || a.name).localeCompare(String(b.stat || b.name)));
    return ordered.concat(tail);
}

function enqueueTerminalSummary(report, context) {
    const summaryTables = getSummaryTables(report);
    if (!summaryTables.length) return;
    enqueueTerminalLine('RESULTS SUMMARY', 'highlight');
    const ordered = orderSummaryRows(summaryTables, context && context.position);
    ordered.forEach((row) => {
        const statLabel = row.stat || row.name || 'stat';
        const p10 = getRowValue(row, 'p10');
        const p25 = getRowValue(row, 'p25');
        const p50 = getRowValue(row, 'p50');
        const p75 = getRowValue(row, 'p75');
        const p90 = getRowValue(row, 'p90');
        const parts = [];
        if (p10 != null) parts.push(`p10=${formatStatValue(p10)}`);
        if (p25 != null) parts.push(`p25=${formatStatValue(p25)}`);
        if (p50 != null) parts.push(`p50=${formatStatValue(p50)}`);
        if (p75 != null) parts.push(`p75=${formatStatValue(p75)}`);
        if (p90 != null) parts.push(`p90=${formatStatValue(p90)}`);
        if (parts.length > 0) {
            enqueueTerminalLine(`${statLabel}: ${parts.join(', ')}`, 'info');
        }
    });
    const narrative = buildNarrativeLine(summaryTables, context);
    if (narrative) {
        enqueueTerminalLine(narrative, 'info');
    }
}

function buildNarrativeLine(summaryTables, context) {
    const position = normalizePosition(context && context.position);
    const playerName = context && context.player_name ? context.player_name : 'This player';
    const getP50 = (names) => {
        const row = getStatRow(summaryTables, names);
        const value = row ? getRowValue(row, 'p50') : null;
        return value == null ? null : formatStatValue(value);
    };
    if (position === 'QB') {
        const completions = getP50(['passing_completions', 'completions']);
        const attempts = getP50(['passing_attempts', 'attempts']);
        const passYards = getP50(['passing_yards']);
        const passTDs = getP50(['passing_tds', 'pass_tds']);
        const rushYards = getP50(['qb_rush_yards', 'rushing_yards']);
        const parts = [];
        if (completions && attempts) parts.push(`~${completions} completions on ${attempts} attempts`);
        if (passYards) parts.push(`${passYards} passing yards`);
        if (passTDs) parts.push(`${passTDs} passing TDs`);
        if (rushYards) parts.push(`${rushYards} rush yards`);
        if (parts.length === 0) return '';
        return `The simulator projects ${playerName} for ${joinClauses(parts)}.`;
    }
    if (position === 'RB') {
        const rushYards = getP50(['rushing_yards', 'rush_yards']);
        const receptions = getP50(['receptions']);
        const recYards = getP50(['receiving_yards']);
        const tds = getP50(['total_touchdowns']);
        const parts = [];
        if (rushYards) parts.push(`${rushYards} rush yards`);
        if (receptions && recYards) parts.push(`${receptions} receptions for ${recYards} yards`);
        if (tds) parts.push(`${tds} total TDs`);
        if (parts.length === 0) return '';
        return `The simulator projects ${playerName} for ${joinClauses(parts)}.`;
    }
    if (position === 'WR' || position === 'TE') {
        const targets = getP50(['targets']);
        const receptions = getP50(['receptions']);
        const recYards = getP50(['receiving_yards']);
        const recTDs = getP50(['total_touchdowns']);
        const parts = [];
        if (receptions && targets) parts.push(`~${receptions} catches on ${targets} targets`);
        if (recYards) parts.push(`${recYards} receiving yards`);
        if (recTDs) parts.push(`${recTDs} total TDs`);
        if (parts.length === 0) return '';
        return `The simulator projects ${playerName} for ${joinClauses(parts)}.`;
    }
    if (position === 'K') {
        const fgMade = getP50(['fg_made', 'field_goals_made']);
        const xpMade = getP50(['pat_made', 'extra_points_made']);
        const parts = [];
        if (fgMade) parts.push(`${fgMade} field goals`);
        if (xpMade) parts.push(`${xpMade} extra points`);
        if (parts.length === 0) return '';
        return `The simulator projects ${playerName} for ${joinClauses(parts)}.`;
    }
    return '';
}

function joinClauses(parts) {
    if (parts.length === 1) return parts[0];
    if (parts.length === 2) return `${parts[0]} and ${parts[1]}`;
    return `${parts.slice(0, -1).join(', ')}, and ${parts[parts.length - 1]}`;
}

function enqueueTerminalDiagnostics(report, context) {
    const metadata = report && report.metadata ? report.metadata : {};
    const lines = [];
    const nSims = metadata.n_sims || context.n_sims;
    const seed = metadata.seed || context.seed;
    const mode = metadata.mode || context.mode;
    if (nSims != null || seed != null || mode) {
        lines.push(`Run diagnostics: n_sims=${nSims ?? '—'}, seed=${seed ?? '—'}, mode=${mode || '—'}`);
    }
    const opponent = metadata.opponent || context.opponent;
    const homeAway = metadata.home_away || metadata.schedule_home_away || '';
    if (opponent) {
        lines.push(`Opponent context: ${homeAway ? `${homeAway} ` : ''}${opponent}`);
    }
    const trainingRows = metadata.training_rows || metadata.training_row_count || metadata.training_rows_count;
    if (trainingRows != null) {
        lines.push(`Training rows: ${trainingRows}`);
    }
    if (lines.length > 0) {
        enqueueTerminalLine('RUN DIAGNOSTICS', 'highlight');
        lines.forEach((line) => enqueueTerminalLine(line, 'info'));
    }
}

function trimChartPayload(chart, maxPoints) {
    if (!chart || typeof chart !== 'object') return chart;
    const trimmed = Array.isArray(chart) ? chart.slice(0, maxPoints) : { ...chart };
    if (Array.isArray(trimmed.points)) trimmed.points = trimmed.points.slice(0, maxPoints);
    if (Array.isArray(trimmed.bins)) trimmed.bins = trimmed.bins.slice(0, maxPoints);
    if (Array.isArray(trimmed.values)) trimmed.values = trimmed.values.slice(0, maxPoints);
    if (Array.isArray(trimmed.x)) trimmed.x = trimmed.x.slice(0, maxPoints);
    if (Array.isArray(trimmed.y)) trimmed.y = trimmed.y.slice(0, maxPoints);
    if (Array.isArray(trimmed.series)) {
        trimmed.series = trimmed.series.map(series => trimChartPayload(series, maxPoints));
    }
    return trimmed;
}

function compactCharts(charts, maxPoints = 120) {
    if (!charts) return null;
    if (Array.isArray(charts)) return charts.map(chart => trimChartPayload(chart, maxPoints));
    if (typeof charts === 'object') {
        const out = {};
        Object.entries(charts).forEach(([key, chart]) => {
            out[key] = trimChartPayload(chart, maxPoints);
        });
        return out;
    }
    return null;
}

function saveRunToStorage(payload, report) {
    if (!report || !report.metadata) return;
    const runId = `run_${Date.now()}_${simulationRunCounter}`;
    const summaryTables = getSummaryTables(report);
    const charts = compactCharts(report.charts);
    const record = {
        run_id: runId,
        run_date: new Date().toISOString(),
        request: {
            player_id: payload.player_id || null,
            player_name: payload.player_name || null,
            season: payload.season || null,
            week: payload.week || null,
            mode: payload.mode || null,
            opponent: payload.schedule_opponent || null,
            n_sims: payload.n_sims || null,
            seed: payload.seed || null
        },
        metadata: {
            player_id: report.metadata.player_id || payload.player_id || null,
            player_name: report.metadata.player_name || payload.player_name || null,
            position: report.metadata.position || null,
            opponent: report.metadata.opponent || payload.schedule_opponent || null,
            home_away: report.metadata.home_away || payload.schedule_home_away || null,
            season: report.metadata.season || payload.season || null,
            week: report.metadata.week || payload.week || null,
            mode: report.metadata.mode || payload.mode || null,
            n_sims: report.metadata.n_sims || payload.n_sims || null,
            seed: report.metadata.seed || payload.seed || null
        },
        summary_tables: summaryTables,
        charts: charts
    };

    const MAX_RECORD_BYTES = 200 * 1024;
    let serialized = JSON.stringify(record);
    if (serialized.length > MAX_RECORD_BYTES) {
        record.charts = null;
        serialized = JSON.stringify(record);
    }
    if (serialized.length > MAX_RECORD_BYTES) {
        record.summary_tables = [];
        record.note = 'results too large to cache locally';
        serialized = JSON.stringify(record);
    }

    const RUN_INDEX_KEY = 'rtf_run_index_v1';
    const RUN_RECORD_PREFIX = 'rtf_run_';
    const MAX_RUNS = 10;
    const RUNS_KEY = 'rtf_runs_v1';

    const existingIndex = JSON.parse(localStorage.getItem(RUN_INDEX_KEY) || '[]')
        .filter((id) => typeof id === 'string' && id.startsWith('run_'));
    const nextIndex = [runId, ...existingIndex.filter((id) => id !== runId)].slice(0, MAX_RUNS);

    const persist = () => {
        const stats = buildRunStats(record.metadata.position, summaryTables);
        const listEntry = {
            run_id: runId,
            run_date: record.run_date,
            player_name: record.metadata.player_name || '',
            player_id: record.metadata.player_id || '',
            position: record.metadata.position || '',
            opponent: record.metadata.opponent || '',
            season: record.metadata.season || record.request.season,
            week: record.metadata.week || record.request.week,
            mode: record.metadata.mode || record.request.mode,
            stats: stats,
            fantasy_points: stats.fantasy_points || null
        };
        const existingRuns = JSON.parse(localStorage.getItem(RUNS_KEY) || '[]');
        existingRuns.unshift(listEntry);
        localStorage.setItem(RUNS_KEY, JSON.stringify(existingRuns.slice(0, MAX_RUNS)));
        localStorage.setItem(RUN_INDEX_KEY, JSON.stringify(nextIndex));
        localStorage.setItem(`${RUN_RECORD_PREFIX}${runId}`, serialized);
        existingIndex.filter((oldId) => !nextIndex.includes(oldId)).forEach((oldId) => {
            localStorage.removeItem(`${RUN_RECORD_PREFIX}${oldId}`);
        });
        console.info('[RTF] cached run', { runId, bytes: serialized.length });
    };

    try {
        persist();
    } catch (err) {
        existingIndex.forEach((oldId) => {
            localStorage.removeItem(`${RUN_RECORD_PREFIX}${oldId}`);
        });
        try {
            localStorage.setItem(RUN_INDEX_KEY, JSON.stringify([runId]));
            localStorage.setItem(`${RUN_RECORD_PREFIX}${runId}`, serialized);
            console.info('[RTF] cached run after prune', { runId, bytes: serialized.length });
        } catch (retryErr) {
            console.warn('[RTF] localStorage disabled for runs', retryErr);
        }
    }
}

function loadRunFromStorage(runId) {
    const reportRaw = localStorage.getItem(`rtf_run_${runId}`);
    if (!reportRaw) return;
    try {
        const report = JSON.parse(reportRaw);
        if (report && report.metadata) {
            renderResults(report);
        }
    } catch (err) {
        console.error('[RTF] Failed to load run', err);
    }
}

// Update Calculate Button State
function updateCalculateButton() {
    const simulationButton = document.getElementById('simulation-button');
    if (!simulationButton) return;

    const resolved = resolvePayload();
    simulationButton.disabled = !resolved.valid;
}

// Simulation Button
function initializeSimulationButton() {
    const simulationButton = document.getElementById('simulation-button');
    if (!simulationButton) return;

    const handleActivate = () => {
        const isValid = selectedPlayer && selectedSeason && selectedWeek;
        
        if (!isValid) {
            return;
        }

        // Hide previous results
        const statsSection = document.getElementById('rtf-stats-section');
        const distributionsSection = document.getElementById('rtf-distributions-section');
        if (statsSection) statsSection.style.display = 'none';
        if (distributionsSection) distributionsSection.style.display = 'none';

        // Show and start terminal
        const terminalContainer = document.getElementById('terminal-container');
        if (terminalContainer) {
            terminalContainer.style.display = 'block';
            scrollToTerminal();
            runSimulationRequest();
        }
    };

    simulationButton.addEventListener('click', handleActivate);
    
    // Keyboard accessibility
    simulationButton.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' || e.key === ' ') {
            e.preventDefault();
            handleActivate();
        }
    });
    
    // Ensure button has proper ARIA attributes
    simulationButton.setAttribute('role', 'button');
    simulationButton.setAttribute('tabindex', '0');
}

async function runSimulationRequest() {
    const terminalOutput = document.getElementById('terminalOutput');
    const simulationButton = document.getElementById('simulation-button');

    if (!terminalOutput) return;
    if (simulationInFlight) return;
    simulationInFlight = true;
    simulationRunCounter += 1;
    terminalRunToken = simulationRunCounter;
    terminalQueue = [];
    terminalFlushActive = false;
    clearTerminal();
    if (simulationButton) {
        simulationButton.disabled = true;
        simulationButton.textContent = 'RUNNING...';
    }

    const resolved = resolvePayload();
    if (!resolved.valid) {
        enqueueTerminalLine(`ERROR: ${resolved.message || 'Missing required inputs.'}`, 'error');
        flushTerminalQueue(terminalRunToken);
        if (simulationButton) {
            simulationButton.disabled = false;
            simulationButton.textContent = 'RUN SIMULATION';
        }
        simulationInFlight = false;
        return;
    }

    try {
        console.info('[RTF] simulate payload', resolved.payload);
        window.lastSimulationPayload = resolved.payload;
        const context = buildTerminalContext(resolved.payload);
        const rng = makeSeededRng(context.seed);
        terminalRng = rng;
        emitPhaseMessages('initialization', rng, context);
        emitPhaseMessages('context_build', rng, context);
        emitPhaseMessages('model_setup', rng, context);
        flushTerminalQueue(terminalRunToken);
        const requestPromise = fetchJson('/simulate', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(resolved.payload)
        });
        emitPhaseMessages('monte_carlo', rng, context, true);
        flushTerminalQueue(terminalRunToken);
        const payload = await requestPromise;
        saveRunToStorage(resolved.payload, payload);
        emitPhaseMessages('aggregation', rng, context);
        enqueueTerminalLine('Simulation response received.', 'success');
        enqueueTerminalSummary(payload, context);
        enqueueTerminalDiagnostics(payload, context);
        renderResults(payload);
        emitPhaseMessages('finalize', rng, context);
        const events = payload && payload.diagnostics && payload.diagnostics.terminal_events;
        if (Array.isArray(events)) {
            events.forEach((evt) => {
                if (!evt) return;
                const msg = evt.message || evt.text || evt.code || 'event';
                enqueueTerminalLine(msg, 'muted', '\u21b3 ');
            });
        }
        flushTerminalQueue(terminalRunToken);
    } catch (err) {
        enqueueTerminalLine(
            `ERROR: ${err && err.message ? err.message : 'Unable to reach the simulation service.'}`,
            'error'
        );
        flushTerminalQueue(terminalRunToken);
    } finally {
        if (simulationButton) {
            simulationButton.disabled = false;
            simulationButton.textContent = 'RUN SIMULATION';
        }
        simulationInFlight = false;
    }
}

function renderError(message) {
    const statsSection = document.getElementById('rtf-stats-section');
    const statsContainer = document.getElementById('stats-table-container');
    const distributionsSection = document.getElementById('rtf-distributions-section');
    if (statsSection && statsContainer) {
        statsSection.style.display = 'block';
        statsContainer.innerHTML = `<div class="result-row error">${message}</div>`;
    }
    if (distributionsSection) {
        distributionsSection.style.display = 'none';
    }
}

function renderResults(result) {
    const statsSection = document.getElementById('rtf-stats-section');
    const statsContainer = document.getElementById('stats-table-container');
    const distributionsSection = document.getElementById('rtf-distributions-section');
    const distributionsContainer = document.getElementById('distributions-container');
    if (!statsSection || !statsContainer) return;

    const summaryTables = getSummaryTables(result);
    const metadata = result && result.metadata ? result.metadata : {};
    if (!summaryTables || summaryTables.length === 0) {
        renderError('Simulation returned no summary tables.');
        return;
    }

    statsContainer.innerHTML = '';
    const header = document.createElement('div');
    header.className = 'result-row';
    header.innerHTML = `<strong>${metadata.player_name || (selectedPlayer ? selectedPlayer.player_name : 'Player')}</strong> | ${metadata.season || ''} Week ${metadata.week || ''}`;
    statsContainer.appendChild(header);
    statsContainer.appendChild(buildStatsTable(summaryTables, metadata.position || (selectedPlayer ? selectedPlayer.position : '')));
    statsSection.style.display = 'block';

    const quantileLookup = buildQuantileLookup(summaryTables);
    if (distributionsSection && distributionsContainer) {
        const charts = normalizeChartsPayload(result.charts);
        distributionsContainer.innerHTML = '';
        if (charts.length === 0) {
            distributionsSection.style.display = 'none';
        } else {
            charts.forEach((chart) => {
                const card = document.createElement('div');
                card.className = 'distribution-card';
                const title = document.createElement('h4');
                title.textContent = chart.stat || chart.name || 'Distribution';
                card.appendChild(title);
                const statKey = normalizeStatKey(chart.stat || chart.name || '');
                const quantiles = quantileLookup.get(statKey) || null;
                const svg = renderChartSVG(chart, quantiles);
                if (svg) {
                    card.appendChild(svg);
                }
                const caption = document.createElement('div');
                caption.className = 'distribution-caption';
                caption.textContent = 'Distribution of simulated outcomes';
                card.appendChild(caption);
                distributionsContainer.appendChild(card);
            });
            distributionsSection.style.display = 'block';
        }
    }
}

function buildStatsTable(summaryTables, position) {
    const percentiles = ['p10', 'p25', 'p40', 'p50', 'p60', 'p75', 'p90'];
    const table = document.createElement('table');
    table.className = 'stats-table';
    table.innerHTML = `
        <thead>
            <tr>
                <th>Stat</th>
                ${percentiles.map(p => `<th>${p.toUpperCase()}</th>`).join('')}
            </tr>
        </thead>
        <tbody></tbody>
    `;
    const tbody = table.querySelector('tbody');
    const ordered = orderSummaryRows(summaryTables, position || (selectedPlayer ? selectedPlayer.position : ''));
    ordered.forEach((row) => {
        const tr = document.createElement('tr');
        const statLabel = row.stat || row.name || 'stat';
        const cells = percentiles.map((p) => {
            const value = getRowValue(row, p);
            return `<td>${value == null ? '—' : formatStatValue(value)}</td>`;
        });
        tr.innerHTML = `<td>${statLabel}</td>${cells.join('')}`;
        tbody.appendChild(tr);
    });
    return table;
}

function buildQuantileLookup(summaryTables) {
    const lookup = new Map();
    summaryTables.forEach((row) => {
        const key = normalizeStatKey(row.stat || row.name || '');
        if (!key) return;
        lookup.set(key, {
            p10: getRowValue(row, 'p10'),
            p25: getRowValue(row, 'p25'),
            p40: getRowValue(row, 'p40'),
            p50: getRowValue(row, 'p50'),
            p60: getRowValue(row, 'p60'),
            p75: getRowValue(row, 'p75'),
            p90: getRowValue(row, 'p90')
        });
    });
    return lookup;
}

function normalizeChartsPayload(charts) {
    if (!charts) return [];
    if (Array.isArray(charts)) return charts;
    if (typeof charts === 'object') {
        return Object.entries(charts).map(([key, value]) => {
            if (value && typeof value === 'object') {
                return { stat: value.stat || value.name || key, ...value };
            }
            return { stat: key, value };
        });
    }
    return [];
}

function extractChartPoints(chart) {
    if (!chart || typeof chart !== 'object') return null;
    if (Array.isArray(chart.points)) return chart.points;
    if (Array.isArray(chart.density)) return chart.density;
    if (Array.isArray(chart.series) && chart.series.length > 0) {
        if (Array.isArray(chart.series[0].points)) return chart.series[0].points;
        if (Array.isArray(chart.series[0].x) && Array.isArray(chart.series[0].y)) {
            return chart.series[0].x.map((x, idx) => ({ x, y: chart.series[0].y[idx] }));
        }
    }
    if (Array.isArray(chart.x) && Array.isArray(chart.y)) {
        return chart.x.map((x, idx) => ({ x, y: chart.y[idx] }));
    }
    return null;
}

function extractHistogramBins(chart) {
    if (!chart || typeof chart !== 'object') return null;
    if (Array.isArray(chart.bins)) return chart.bins;
    if (Array.isArray(chart.histogram)) return chart.histogram;
    return null;
}

function estimatePercentile(x, quantiles) {
    if (!quantiles) return null;
    const points = [
        { p: 0.10, x: quantiles.p10 },
        { p: 0.25, x: quantiles.p25 },
        { p: 0.40, x: quantiles.p40 },
        { p: 0.50, x: quantiles.p50 },
        { p: 0.60, x: quantiles.p60 },
        { p: 0.75, x: quantiles.p75 },
        { p: 0.90, x: quantiles.p90 }
    ].filter((pt) => typeof pt.x === 'number' && !Number.isNaN(pt.x));
    if (points.length < 2) return null;
    points.sort((a, b) => a.x - b.x);
    if (x <= points[0].x) return points[0].p;
    if (x >= points[points.length - 1].x) return points[points.length - 1].p;
    for (let i = 0; i < points.length - 1; i++) {
        const left = points[i];
        const right = points[i + 1];
        if (x >= left.x && x <= right.x) {
            const span = right.x - left.x || 1;
            const t = (x - left.x) / span;
            return left.p + t * (right.p - left.p);
        }
    }
    return null;
}

function renderChartSVG(chart, quantiles) {
    const width = 260;
    const height = 120;
    const padding = 10;
    const wrapper = document.createElement('div');
    wrapper.style.position = 'relative';
    const tooltip = document.createElement('div');
    tooltip.style.position = 'absolute';
    tooltip.style.pointerEvents = 'none';
    tooltip.style.background = '#111';
    tooltip.style.color = '#cfe6cf';
    tooltip.style.border = '1px solid #2f3a2f';
    tooltip.style.padding = '2px 6px';
    tooltip.style.fontSize = '0.5rem';
    tooltip.style.opacity = '0';
    tooltip.style.transition = 'opacity 120ms ease';
    tooltip.style.whiteSpace = 'nowrap';
    wrapper.appendChild(tooltip);

    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
    svg.setAttribute('width', width);
    svg.setAttribute('height', height);
    svg.style.cursor = 'crosshair';

    const bins = extractHistogramBins(chart);
    if (bins && bins.length > 0) {
        const values = bins.map((bin) => {
            if (typeof bin === 'number') return bin;
            return bin.count ?? bin.y ?? bin.value ?? 0;
        });
        const maxValue = Math.max(...values, 1);
        const barWidth = (width - padding * 2) / bins.length;
        bins.forEach((bin, idx) => {
            const value = values[idx];
            const barHeight = (value / maxValue) * (height - padding * 2);
            const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
            rect.setAttribute('x', padding + idx * barWidth);
            rect.setAttribute('y', height - padding - barHeight);
            rect.setAttribute('width', Math.max(1, barWidth - 1));
            rect.setAttribute('height', barHeight);
            rect.setAttribute('fill', '#7cb342');
            svg.appendChild(rect);
        });
        wrapper.appendChild(svg);
        return wrapper;
    }

    const points = extractChartPoints(chart);
    if (!points || points.length === 0) return null;
    const xs = points.map((p) => p.x ?? p[0]).filter((v) => typeof v === 'number');
    const ys = points.map((p) => p.y ?? p[1]).filter((v) => typeof v === 'number');
    const minX = Math.min(...xs, 0);
    const maxX = Math.max(...xs, 1);
    const minY = Math.min(...ys, 0);
    const maxY = Math.max(...ys, 1);
    const scaleX = (x) => padding + ((x - minX) / (maxX - minX || 1)) * (width - padding * 2);
    const scaleY = (y) => height - padding - ((y - minY) / (maxY - minY || 1)) * (height - padding * 2);
    const polyline = document.createElementNS('http://www.w3.org/2000/svg', 'polyline');
    const linePoints = points.map((p, idx) => {
        const xVal = p.x ?? p[0] ?? xs[idx] ?? 0;
        const yVal = p.y ?? p[1] ?? ys[idx] ?? 0;
        return `${scaleX(xVal)},${scaleY(yVal)}`;
    });
    polyline.setAttribute('points', linePoints.join(' '));
    polyline.setAttribute('fill', 'none');
    polyline.setAttribute('stroke', '#7cb342');
    polyline.setAttribute('stroke-width', '2');
    svg.appendChild(polyline);
    const guide = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    guide.setAttribute('y1', padding);
    guide.setAttribute('y2', height - padding);
    guide.setAttribute('stroke', '#f5d76e');
    guide.setAttribute('stroke-width', '1');
    guide.setAttribute('opacity', '0');
    svg.appendChild(guide);

    svg.addEventListener('mousemove', (event) => {
        const rect = svg.getBoundingClientRect();
        const xPix = event.clientX - rect.left;
        const xValue = minX + ((xPix / rect.width) * (maxX - minX));
        const percentile = estimatePercentile(xValue, quantiles);
        const xSvg = Math.max(padding, Math.min(width - padding, xPix * (width / rect.width)));
        guide.setAttribute('x1', xSvg);
        guide.setAttribute('x2', xSvg);
        guide.setAttribute('opacity', '0.9');
        const pctLabel = percentile == null ? '—' : `P${Math.round(percentile * 100)}`;
        tooltip.textContent = `value ≈ ${formatStatValue(xValue)} | percentile ≈ ${pctLabel}`;
        tooltip.style.left = `${Math.min(xPix + 8, rect.width - 120)}px`;
        tooltip.style.top = '6px';
        tooltip.style.opacity = '1';
    });

    svg.addEventListener('mouseleave', () => {
        guide.setAttribute('opacity', '0');
        tooltip.style.opacity = '0';
    });

    wrapper.appendChild(svg);
    return wrapper;
}

// Terminal Animation
function startTerminalAnimation() {
    const terminalOutput = document.getElementById('terminalOutput');
    
    if (!terminalOutput) return;

    // Clear previous output
    terminalOutput.innerHTML = '<div class="terminal-line-wrapper"><span class="terminal-cursor" id="terminalCursor">_</span></div>';
    
    // Get cursor reference after recreating HTML
    const terminalCursor = document.getElementById('terminalCursor');

    const playerName = selectedPlayer.player_name;
    const playerPos = selectedPlayer.position;
    const opponentName = getTeamName(selectedTeam);
    const location = homeAway === 'vs' ? 'Home' : 'Away';

    // Determine game context based on mode
    let gameContextText = 'Week 14, 2024'; // Default
    if (simulationMode === 'historical' && selectedDate) {
        const dateObj = new Date(selectedDate);
        gameContextText = dateObj.toLocaleDateString('en-US', { 
            year: 'numeric', 
            month: 'long', 
            day: 'numeric' 
        });
    } else if (simulationMode === 'this-week') {
        gameContextText = 'This Week\'s Game';
    } else if (simulationMode === 'hypothetical') {
        gameContextText = 'Hypothetical Matchup';
    }

    // Position-based stat definitions (from README)
    const positionStats = {
        'QB': ['PYD', 'PTD', 'RYD', 'RTD', 'P Attempts', 'R Attempts', 'Completions'],
        'RB': ['RUYD', 'RECYD', 'TOTTD', 'Carries'],
        'WR': ['Receptions', 'RECYD', 'RECTD'],
        'TE': ['Receptions', 'RECYD', 'RECTD'],
        'OL': [], // No returned stats
        'DT': ['Tackles', 'Sacks', 'FF'],
        'DE': ['Tackles', 'Sacks', 'FF'],
        'S': ['Sacks', 'Tackles', 'Interceptions', 'Passes Defended'],
        'CB': ['Sacks', 'Tackles', 'Interceptions', 'Passes Defended'],
        'LB': ['Sacks', 'Tackles', 'FF'],
        'K': ['FG Made', 'FG Attempts', '# PAT'],
        'P': ['# Punts', 'Punts within 10/20 yards']
    };

    // Get stats for this position
    const stats = positionStats[playerPos] || ['PPR Fantasy']; // Default to fantasy if position not found

    const terminalLines = [
        { text: 'READ THE FIELD - MODELING ENGINE v2.1.4', delay: 0, class: 'highlight' },
        { text: '==========================================', delay: 200, class: 'info' },
        { text: '', delay: 400, class: 'info' },
        { text: '> Initializing simulation pipeline...', delay: 600, class: 'prompt' },
        { text: '', delay: 800, class: 'info' },
        { text: '[1/7] Loading historical NFL data...', delay: 1000, class: 'info' },
        { text: '    Scanning data/cache/player_week_identity.parquet', delay: 1200, class: 'info' },
        { text: '    Scanning data/processed/defense_weekly_features.parquet', delay: 1400, class: 'info' },
        { text: '    Scanning data/processed/team_offense_context.parquet', delay: 1600, class: 'info' },
        { text: '    Loaded cached player-week identity', delay: 1800, class: 'success' },
        { text: '    Loaded defensive matchup features', delay: 2000, class: 'success' },
        { text: '    Loaded team offense context', delay: 2200, class: 'success' },
        { text: '', delay: 2400, class: 'info' },
        { text: '[2/7] Identifying target player and matchup...', delay: 2600, class: 'info' },
        { text: `    Player: ${playerName} (${playerPos})`, delay: 2800, class: 'info' },
        { text: `    Opponent: ${opponentName} (${location})`, delay: 3000, class: 'info' },
        { text: `    Game context: ${gameContextText}`, delay: 3200, class: 'info' },
        { text: '', delay: 3400, class: 'info' },
        { text: '[3/7] Building feature set...', delay: 3600, class: 'info' },
        { text: '    Computing recent usage metrics (last 4 games)', delay: 3800, class: 'info' },
        { text: `    Computing matchup features (opponent defense vs ${playerPos})`, delay: 4000, class: 'info' },
        { text: '    Computing game context features (weather, line, pace)', delay: 4200, class: 'info' },
        { text: '    Feature vector: 47 dimensions', delay: 4400, class: 'success' },
        { text: '', delay: 4600, class: 'info' },
        { text: '[4/7] Training ensemble models...', delay: 4800, class: 'info' },
        { text: '    Initializing Random Forest (n_estimators=200)', delay: 5000, class: 'info' },
        { text: '    Initializing Gradient Boosting (n_estimators=150)', delay: 5200, class: 'info' },
        { text: '    Initializing XGBoost (n_estimators=100)', delay: 5400, class: 'info' },
        { text: '    Training models on historical data...', delay: 5600, class: 'info' },
        { text: '    [████████████████████] 100%', delay: 5800, class: 'success' },
        { text: '    Models trained successfully', delay: 6000, class: 'success' },
        { text: '', delay: 6200, class: 'info' },
        { text: '[5/7] Model statistics:', delay: 6400, class: 'info' },
        { text: '    Total trees trained: 450', delay: 6600, class: 'highlight' },
        { text: '    Feature importance: usage (0.34), matchup (0.28), context (0.22)', delay: 7000, class: 'info' },
        { text: '', delay: 7400, class: 'info' },
        { text: '[6/7] Running Monte Carlo simulations...', delay: 7600, class: 'info' },
        { text: '    Generating outcome distributions...', delay: 7800, class: 'info' },
        { text: '    Number of simulations: 10,000', delay: 8000, class: 'highlight' },
        { text: '    Accounting for model uncertainty...', delay: 8200, class: 'info' },
        { text: '    Accounting for residual variance...', delay: 8400, class: 'info' },
        { text: '    Accounting for matchup volatility...', delay: 8600, class: 'info' },
        { text: '', delay: 8800, class: 'info' },
        { text: '[7/7] Summarizing results...', delay: 9000, class: 'info' },
        { text: '', delay: 9200, class: 'info' },
        { text: 'PROJECTION SUMMARY', delay: 9400, class: 'highlight' },
        { text: '==================', delay: 9600, class: 'info' },
        { text: '', delay: 9800, class: 'info' },
        { text: '> Simulation complete.', delay: 10000, class: 'prompt' },
        { text: '', delay: 10200, class: 'info' }
    ];

    let currentLineIndex = 0;
    const typingSpeed = 10;
    const lineDelay = 30;

    function createLineElement(lineData) {
        const line = document.createElement('div');
        line.className = `terminal-line ${lineData.class || 'info'}`;
        line.textContent = '';
        return line;
    }

    function typeText(element, text, callback) {
        if (!text) {
            callback();
            return;
        }

        let charIndex = 0;
        function typeChar() {
            if (charIndex < text.length) {
                element.textContent += text[charIndex];
                charIndex++;
                setTimeout(typeChar, typingSpeed);
            } else {
                callback();
            }
        }
        typeChar();
    }

    function addLine(lineData) {
        return new Promise((resolve) => {
            const lineElement = createLineElement(lineData);
            const cursorWrapper = terminalOutput.querySelector('.terminal-line-wrapper');
            if (cursorWrapper && cursorWrapper.parentNode === terminalOutput) {
                terminalOutput.insertBefore(lineElement, cursorWrapper);
            } else {
                terminalOutput.appendChild(lineElement);
            }
            
            const scrollToBottom = () => {
                terminalOutput.scrollTop = terminalOutput.scrollHeight;
            };
            
            typeText(lineElement, lineData.text, () => {
                setTimeout(() => {
                    scrollToBottom();
                    resolve();
                }, lineDelay);
            });
        });
    }

    async function runAnimation() {
        let lastDelay = 0;
        for (let i = 0; i < terminalLines.length; i++) {
            const lineData = terminalLines[i];
            const relativeDelay = lineData.delay - lastDelay;
            if (relativeDelay > 0) {
                await new Promise(resolve => setTimeout(resolve, relativeDelay));
            }
            lastDelay = lineData.delay;
            await addLine(lineData);
        }

        // Hide cursor when done
        if (terminalCursor) {
            terminalCursor.style.display = 'none';
        }

        // Show results panel with position-based stats
        const resultsPanel = document.getElementById('results-panel');
        const resultsContent = document.getElementById('results-content');
        if (resultsPanel && resultsContent) {
            resultsPanel.style.display = 'block';
            
            // Clear previous results
            resultsContent.innerHTML = '';
            
            // Generate mock predicted values and percentiles for each stat
            const generateStatData = (statName) => {
                let median, p10, p25, p75, p90;
                
                // Generate realistic mock values based on stat type
                if (statName.includes('YD') || statName === 'PYD' || statName === 'RYD' || statName === 'RECYD' || statName === 'RUYD') {
                    median = Math.floor(Math.random() * 150) + 20; // Yards: 20-170
                    p10 = Math.max(0, Math.floor(median * 0.6));
                    p25 = Math.floor(median * 0.75);
                    p75 = Math.floor(median * 1.25);
                    p90 = Math.floor(median * 1.5);
                } else if (statName.includes('TD') || statName === 'PTD' || statName === 'RTD' || statName === 'RECTD' || statName === 'TOTTD') {
                    median = Math.floor(Math.random() * 3); // Touchdowns: 0-2
                    p10 = Math.max(0, median - 1);
                    p25 = Math.max(0, median - 1);
                    p75 = median + 1;
                    p90 = median + 2;
                } else if (statName === 'Receptions') {
                    median = Math.floor(Math.random() * 10) + 2; // Receptions: 2-11
                    p10 = Math.max(0, Math.floor(median * 0.6));
                    p25 = Math.floor(median * 0.8);
                    p75 = Math.floor(median * 1.3);
                    p90 = Math.floor(median * 1.6);
                } else if (statName === 'Carries' || statName === 'P Attempts' || statName === 'R Attempts') {
                    median = Math.floor(Math.random() * 25) + 5; // Attempts: 5-29
                    p10 = Math.max(0, Math.floor(median * 0.65));
                    p25 = Math.floor(median * 0.8);
                    p75 = Math.floor(median * 1.25);
                    p90 = Math.floor(median * 1.5);
                } else if (statName === 'Completions') {
                    median = Math.floor(Math.random() * 20) + 10; // Completions: 10-29
                    p10 = Math.max(0, Math.floor(median * 0.7));
                    p25 = Math.floor(median * 0.85);
                    p75 = Math.floor(median * 1.2);
                    p90 = Math.floor(median * 1.4);
                } else if (statName === 'Tackles') {
                    median = Math.floor(Math.random() * 12) + 3; // Tackles: 3-14
                    p10 = Math.max(0, Math.floor(median * 0.6));
                    p25 = Math.floor(median * 0.75);
                    p75 = Math.floor(median * 1.3);
                    p90 = Math.floor(median * 1.6);
                } else if (statName === 'Sacks') {
                    median = Math.floor(Math.random() * 2); // Sacks: 0-1
                    p10 = 0;
                    p25 = 0;
                    p75 = median + 1;
                    p90 = median + 2;
                } else if (statName === 'FF') {
                    median = Math.floor(Math.random() * 2); // Forced Fumbles: 0-1
                    p10 = 0;
                    p25 = 0;
                    p75 = median + 1;
                    p90 = median + 1;
                } else if (statName === 'Interceptions') {
                    median = Math.floor(Math.random() * 2); // Interceptions: 0-1
                    p10 = 0;
                    p25 = 0;
                    p75 = median + 1;
                    p90 = median + 2;
                } else if (statName === 'Passes Defended') {
                    median = Math.floor(Math.random() * 4); // Passes Defended: 0-3
                    p10 = 0;
                    p25 = Math.max(0, median - 1);
                    p75 = median + 1;
                    p90 = median + 2;
                } else if (statName === 'FG Made') {
                    median = Math.floor(Math.random() * 3) + 1; // FG Made: 1-3
                    p10 = Math.max(0, median - 1);
                    p25 = Math.max(1, median - 1);
                    p75 = median + 1;
                    p90 = median + 2;
                } else if (statName === 'FG Attempts') {
                    median = Math.floor(Math.random() * 3) + 2; // FG Attempts: 2-4
                    p10 = Math.max(1, median - 1);
                    p25 = Math.max(1, median - 1);
                    p75 = median + 1;
                    p90 = median + 2;
                } else if (statName === '# PAT') {
                    median = Math.floor(Math.random() * 5) + 2; // PAT: 2-6
                    p10 = Math.max(1, median - 2);
                    p25 = Math.max(1, median - 1);
                    p75 = median + 2;
                    p90 = median + 3;
                } else if (statName === '# Punts') {
                    median = Math.floor(Math.random() * 6) + 2; // Punts: 2-7
                    p10 = Math.max(1, median - 2);
                    p25 = Math.max(1, median - 1);
                    p75 = median + 2;
                    p90 = median + 3;
                } else if (statName === 'Punts within 10/20 yards') {
                    median = Math.floor(Math.random() * 3); // Punts within 10/20: 0-2
                    p10 = 0;
                    p25 = 0;
                    p75 = median + 1;
                    p90 = median + 2;
                } else {
                    median = Math.floor(Math.random() * 20) + 5; // Default: 5-24
                    p10 = Math.max(0, Math.floor(median * 0.6));
                    p25 = Math.floor(median * 0.75);
                    p75 = Math.floor(median * 1.3);
                    p90 = Math.floor(median * 1.6);
                }
                
                return { median, p10, p25, p75, p90 };
            };
            
            // If no stats for position, show default message
            if (stats.length === 0) {
                resultsContent.innerHTML = `
                    <div class="projection-primary">
                        <div class="projection-header">
                            <h3 class="projection-title">NO STATS AVAILABLE</h3>
                        </div>
                        <div class="projection-stats">
                            <div class="projection-stat-item">
                                <div class="projection-stat-label">--</div>
                                <div class="projection-stat-value">--</div>
                            </div>
                        </div>
                    </div>
                `;
                return;
            }
            
            // Generate stat data for all stats
            const statData = stats.map(stat => ({
                name: stat,
                ...generateStatData(stat)
            }));
            
            // Build primary panel (median projections only)
            const primaryPanel = document.createElement('div');
            primaryPanel.className = 'projection-primary';
            primaryPanel.innerHTML = `
                <div class="projection-header">
                    <h3 class="projection-title">What will ${playerName} do?</h3>
                    <div class="projection-matchup">${playerPos} • ${location === 'Home' ? 'vs' : '@'} ${opponentName}</div>
                </div>
                <div class="projection-stats">
                    ${statData.map(stat => `
                        <div class="projection-stat-item">
                            <div class="projection-stat-label">${stat.name}</div>
                            <div class="projection-stat-value">${stat.median}</div>
                        </div>
                    `).join('')}
                </div>
            `;
            resultsContent.appendChild(primaryPanel);
            
            // Build distribution toggle button
            const toggleButton = document.createElement('button');
            toggleButton.className = 'distribution-toggle';
            toggleButton.innerHTML = 'VIEW DISTRIBUTION';
            toggleButton.setAttribute('aria-expanded', 'false');
            resultsContent.appendChild(toggleButton);
            
            // Build secondary panel (percentiles and charts)
            const secondaryPanel = document.createElement('div');
            secondaryPanel.className = 'projection-distribution';
            secondaryPanel.style.display = 'none';
            
            // Create percentile table
            const percentileTable = document.createElement('table');
            percentileTable.className = 'percentile-table';
            percentileTable.innerHTML = `
                <thead>
                    <tr>
                        <th>Stat</th>
                        <th>P10</th>
                        <th>P25</th>
                        <th>P50</th>
                        <th>P75</th>
                        <th>P90</th>
                    </tr>
                </thead>
                <tbody>
                    ${statData.map(stat => `
                        <tr>
                            <td class="percentile-stat-name">${stat.name}</td>
                            <td>${stat.p10}</td>
                            <td>${stat.p25}</td>
                            <td class="percentile-median">${stat.median}</td>
                            <td>${stat.p75}</td>
                            <td>${stat.p90}</td>
                        </tr>
                    `).join('')}
                </tbody>
            `;
            
            secondaryPanel.appendChild(percentileTable);
            resultsContent.appendChild(secondaryPanel);
            
            // Toggle functionality
            toggleButton.addEventListener('click', () => {
                const isExpanded = secondaryPanel.style.display !== 'none';
                secondaryPanel.style.display = isExpanded ? 'none' : 'block';
                toggleButton.setAttribute('aria-expanded', !isExpanded);
                toggleButton.textContent = isExpanded ? 'VIEW DISTRIBUTION' : 'HIDE DISTRIBUTION';
            });
        }
    }

    runAnimation();
}
