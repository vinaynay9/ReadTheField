const API_BASE_URL = window.RTF_API_URL || 'http://localhost:8000';
const NICKNAMES_URL = 'data/nicknames.json';
console.info(`[RTF] API_BASE=${API_BASE_URL}`);
const DEFAULT_N_SIMS = 1000;
const DEFAULT_SEED = 4242;

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

function buildApiUrl(path) {
    if (!path) return API_BASE_URL;
    if (/^https?:\/\//i.test(path)) return path;
    if (path.startsWith('/')) return `${API_BASE_URL}${path}`;
    return `${API_BASE_URL}/${path}`;
}

async function fetchJson(path, options = {}) {
    const url = buildApiUrl(path);
    const response = await fetch(url, options);
    let data = null;
    try {
        data = await response.json();
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
    return data && Object.prototype.hasOwnProperty.call(data, 'data') ? data.data : data;
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
        await loadNicknames();
        const [playersResp, teamsResp, seasonsResp] = await Promise.all([
            fetchJson('/players'),
            fetchJson('/teams'),
            fetchJson('/seasons')
        ]);
        players = playersResp.players || [];
        teams = teamsResp.teams || [];
        availableSeasons = seasonsResp.seasons || [];
        buildTeamNameMap();
        buildPlayerSearchIndex();
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
    initializeModeToggle();
    initializePlayerSearch();
    initializeHomeAwayToggle();
    initializeDateSelection();
    initializeSimulationButton();
    loadInitialData().then(() => {
        switchMode('this-week');
        updateCalculateButton();
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
            throw new Error(data.reason === 'season_complete' ? 'Season complete.' : 'No upcoming game found.');
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
            const disabledStyle = disabled ? 'opacity:0.35; filter:grayscale(1); pointer-events:none;' : '';
            html += `
                <div class="team-item ${teamClass}" data-team-id="${teamId}" style="${disabledStyle}">
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

    dateSelect.innerHTML = '<option value="">-- SELECT DATE --</option>';
    games.forEach(game => {
        const option = document.createElement('option');
        option.value = `${game.season}-wk${game.week}-${game.opponent}`;
        option.dataset.season = game.season;
        option.dataset.week = game.week;
        option.dataset.opponent = game.opponent;
        option.dataset.homeAway = game.home_away;
        option.dataset.gameDate = game.game_date;

        const dateObj = game.game_date ? new Date(game.game_date) : null;
        const formattedDate = dateObj && !isNaN(dateObj.getTime())
            ? dateObj.toLocaleDateString('en-US', { year: 'numeric', month: 'short', day: 'numeric' })
            : 'Date unavailable';
        const opponentName = getTeamName(game.opponent);
        const location = game.home_away === 'AWAY' ? '@' : 'vs';
        option.textContent = `${game.season} Week ${game.week} ${location} ${opponentName} (${formattedDate})`;
        dateSelect.appendChild(option);
    });

    selectedDate = null;
    selectedGame = null;
    dateSelect.value = '';
    updateCalculateButton();
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
            return { valid: false, message: 'Select a historical game.' };
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
        if (!season || !week) {
            return { valid: false, message: 'Select a season and week.' };
        }
        if (!selectedTeam) {
            return { valid: false, message: 'Select an opponent.' };
        }
        availabilityPolicy = 'force_counterfactual';
        mode = 'hypothetical_matchup';
        scheduleOpponent = selectedTeam;
        scheduleHomeAway = homeAway === 'away' ? 'AWAY' : 'HOME';
    }

    return {
        valid: true,
        payload: {
            player_id: selectedPlayer.player_id || null,
            player_name: selectedPlayer.player_name || null,
            season: season,
            week: week,
            n_sims: DEFAULT_N_SIMS,
            seed: DEFAULT_SEED,
            availability_policy: availabilityPolicy,
            schema_version: 'v1',
            mode: mode,
            schedule_opponent: scheduleOpponent,
            schedule_home_away: scheduleHomeAway
        }
    };
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
        const resultsPanel = document.getElementById('results-panel');
        if (resultsPanel) {
            resultsPanel.style.display = 'none';
        }

        // Show and start terminal
        const terminalContainer = document.getElementById('terminal-container');
        if (terminalContainer) {
            terminalContainer.style.display = 'block';
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
    terminalOutput.innerHTML = '<div class="terminal-line">Running simulation...</div>';
    if (simulationButton) {
        simulationButton.disabled = true;
        simulationButton.textContent = 'RUNNING...';
    }

    const resolved = resolvePayload();
    if (!resolved.valid) {
        renderError(resolved.message || 'Missing required inputs.');
        if (simulationButton) {
            simulationButton.disabled = false;
            simulationButton.textContent = 'RUN SIMULATION';
        }
        return;
    }

    try {
        console.info('[RTF] simulate payload', resolved.payload);
        window.lastSimulationPayload = resolved.payload;
        const payload = await fetchJson('/simulate', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(resolved.payload)
        });
        renderResults(payload);
    } catch (err) {
        renderError(err && err.message ? err.message : 'Unable to reach the simulation service.');
    } finally {
        if (simulationButton) {
            simulationButton.disabled = false;
            simulationButton.textContent = 'RUN SIMULATION';
        }
    }
}

function renderError(message) {
    const terminalOutput = document.getElementById('terminalOutput');
    const resultsPanel = document.getElementById('results-panel');
    const resultsContent = document.getElementById('results-content');
    if (terminalOutput) {
        terminalOutput.innerHTML = `<div class="terminal-line error">ERROR: ${message}</div>`;
    }
    if (resultsPanel && resultsContent) {
        resultsPanel.style.display = 'block';
        resultsContent.innerHTML = `<div class="result-row error">${message}</div>`;
    }
}

function renderResults(result) {
    const terminalOutput = document.getElementById('terminalOutput');
    const resultsPanel = document.getElementById('results-panel');
    const resultsContent = document.getElementById('results-content');
    if (!resultsPanel || !resultsContent) return;

    if (terminalOutput) {
        terminalOutput.innerHTML = '<div class="terminal-line success">Simulation complete.</div>';
    }

    const summary = result.summary || [];
    const meta = result.metadata || {};
    if (!summary || summary.length === 0) {
        renderError('Simulation returned no summary.');
        return;
    }
    const header = document.createElement('div');
    header.className = 'result-row';
    header.innerHTML = `<strong>${meta.player_name || selectedPlayer.player_name}</strong> | ${meta.season} Week ${meta.week}`;
    resultsContent.innerHTML = '';
    resultsContent.appendChild(header);

    const table = document.createElement('table');
    table.className = 'results-table';
    table.innerHTML = `
        <thead>
            <tr>
                <th>Stat</th>
                <th>P25</th>
                <th>P50</th>
                <th>P75</th>
            </tr>
        </thead>
        <tbody></tbody>
    `;
    const tbody = table.querySelector('tbody');
    summary.forEach((row) => {
        const tr = document.createElement('tr');
        tr.innerHTML = `
            <td>${row.stat}</td>
            <td>${Math.round(row.p25)}</td>
            <td>${Math.round(row.p50)}</td>
            <td>${Math.round(row.p75)}</td>
        `;
        tbody.appendChild(tr);
    });
    resultsContent.appendChild(table);
    resultsPanel.style.display = 'block';
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
