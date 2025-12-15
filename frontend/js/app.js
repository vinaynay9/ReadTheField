// Mock Data - Players (Offensive Skilled Positions Only: QB, RB, WR, TE, K)
const mockPlayers = [
    // QBs
    { id: 1, name: 'Patrick Mahomes', position: 'QB', team: 'Kansas City Chiefs', initials: 'PM' },
    { id: 2, name: 'Josh Allen', position: 'QB', team: 'Buffalo Bills', initials: 'JA' },
    { id: 3, name: 'Lamar Jackson', position: 'QB', team: 'Baltimore Ravens', initials: 'LJ' },
    { id: 4, name: 'Jalen Hurts', position: 'QB', team: 'Philadelphia Eagles', initials: 'JH' },
    { id: 5, name: 'Joe Burrow', position: 'QB', team: 'Cincinnati Bengals', initials: 'JB' },
    { id: 6, name: 'Dak Prescott', position: 'QB', team: 'Dallas Cowboys', initials: 'DP' },
    { id: 7, name: 'Justin Herbert', position: 'QB', team: 'Los Angeles Chargers', initials: 'JH' },
    { id: 8, name: 'C.J. Stroud', position: 'QB', team: 'Houston Texans', initials: 'CS' },
    // RBs
    { id: 9, name: 'Christian McCaffrey', position: 'RB', team: 'San Francisco 49ers', initials: 'CM' },
    { id: 10, name: 'Derrick Henry', position: 'RB', team: 'Tennessee Titans', initials: 'DH' },
    { id: 11, name: 'Austin Ekeler', position: 'RB', team: 'Los Angeles Chargers', initials: 'AE' },
    { id: 12, name: 'Saquon Barkley', position: 'RB', team: 'New York Giants', initials: 'SB' },
    { id: 13, name: 'Breece Hall', position: 'RB', team: 'New York Jets', initials: 'BH' },
    { id: 14, name: 'Alvin Kamara', position: 'RB', team: 'New Orleans Saints', initials: 'AK' },
    // WRs
    { id: 15, name: 'Justin Jefferson', position: 'WR', team: 'Minnesota Vikings', initials: 'JJ' },
    { id: 16, name: 'Tyreek Hill', position: 'WR', team: 'Miami Dolphins', initials: 'TH' },
    { id: 17, name: 'CeeDee Lamb', position: 'WR', team: 'Dallas Cowboys', initials: 'CL' },
    { id: 18, name: 'Davante Adams', position: 'WR', team: 'Las Vegas Raiders', initials: 'DA' },
    { id: 19, name: 'Cooper Kupp', position: 'WR', team: 'Los Angeles Rams', initials: 'CK' },
    { id: 20, name: 'Stefon Diggs', position: 'WR', team: 'Buffalo Bills', initials: 'SD' },
    { id: 21, name: 'Amon-Ra St. Brown', position: 'WR', team: 'Detroit Lions', initials: 'AS' },
    { id: 22, name: 'A.J. Brown', position: 'WR', team: 'Philadelphia Eagles', initials: 'AB' },
    // TEs
    { id: 23, name: 'Travis Kelce', position: 'TE', team: 'Kansas City Chiefs', initials: 'TK' },
    { id: 24, name: 'Mark Andrews', position: 'TE', team: 'Baltimore Ravens', initials: 'MA' },
    { id: 25, name: 'T.J. Hockenson', position: 'TE', team: 'Detroit Lions', initials: 'TH' },
    { id: 26, name: 'Sam LaPorta', position: 'TE', team: 'Detroit Lions', initials: 'SL' },
    // Kickers
    { id: 27, name: 'Justin Tucker', position: 'K', team: 'Baltimore Ravens', initials: 'JT' },
    { id: 28, name: 'Daniel Carlson', position: 'K', team: 'Las Vegas Raiders', initials: 'DC' },
    { id: 29, name: 'Harrison Butker', position: 'K', team: 'Kansas City Chiefs', initials: 'HB' },
    { id: 30, name: 'Evan McPherson', position: 'K', team: 'Cincinnati Bengals', initials: 'EM' }
];

// Mock Data - Teams grouped by division
const mockTeams = {
    'AFC East': [
        { id: 'BUF', name: 'Buffalo Bills', colorClass: 'team-buffalo' },
        { id: 'MIA', name: 'Miami Dolphins', colorClass: 'team-miami' },
        { id: 'NE', name: 'New England Patriots', colorClass: 'team-new-england' },
        { id: 'NYJ', name: 'New York Jets', colorClass: 'team-ny-jets' }
    ],
    'AFC North': [
        { id: 'BAL', name: 'Baltimore Ravens', colorClass: 'team-baltimore' },
        { id: 'CIN', name: 'Cincinnati Bengals', colorClass: 'team-cincinnati' },
        { id: 'CLE', name: 'Cleveland Browns', colorClass: 'team-cleveland' },
        { id: 'PIT', name: 'Pittsburgh Steelers', colorClass: 'team-pittsburgh' }
    ],
    'AFC South': [
        { id: 'HOU', name: 'Houston Texans', colorClass: 'team-houston' },
        { id: 'IND', name: 'Indianapolis Colts', colorClass: 'team-indianapolis' },
        { id: 'JAX', name: 'Jacksonville Jaguars', colorClass: 'team-jacksonville' },
        { id: 'TEN', name: 'Tennessee Titans', colorClass: 'team-tennessee' }
    ],
    'AFC West': [
        { id: 'DEN', name: 'Denver Broncos', colorClass: 'team-denver' },
        { id: 'KC', name: 'Kansas City Chiefs', colorClass: 'team-kansas-city' },
        { id: 'LV', name: 'Las Vegas Raiders', colorClass: 'team-las-vegas' },
        { id: 'LAC', name: 'Los Angeles Chargers', colorClass: 'team-la-chargers' }
    ],
    'NFC East': [
        { id: 'DAL', name: 'Dallas Cowboys', colorClass: 'team-dallas' },
        { id: 'NYG', name: 'New York Giants', colorClass: 'team-ny-giants' },
        { id: 'PHI', name: 'Philadelphia Eagles', colorClass: 'team-philadelphia' },
        { id: 'WAS', name: 'Washington Commanders', colorClass: 'team-washington' }
    ],
    'NFC North': [
        { id: 'CHI', name: 'Chicago Bears', colorClass: 'team-chicago' },
        { id: 'DET', name: 'Detroit Lions', colorClass: 'team-detroit' },
        { id: 'GB', name: 'Green Bay Packers', colorClass: 'team-green-bay' },
        { id: 'MIN', name: 'Minnesota Vikings', colorClass: 'team-minnesota' }
    ],
    'NFC South': [
        { id: 'ATL', name: 'Atlanta Falcons', colorClass: 'team-atlanta' },
        { id: 'CAR', name: 'Carolina Panthers', colorClass: 'team-carolina' },
        { id: 'NO', name: 'New Orleans Saints', colorClass: 'team-new-orleans' },
        { id: 'TB', name: 'Tampa Bay Buccaneers', colorClass: 'team-tampa-bay' }
    ],
    'NFC West': [
        { id: 'ARI', name: 'Arizona Cardinals', colorClass: 'team-arizona' },
        { id: 'LAR', name: 'Los Angeles Rams', colorClass: 'team-la-rams' },
        { id: 'SF', name: 'San Francisco 49ers', colorClass: 'team-san-francisco' },
        { id: 'SEA', name: 'Seattle Seahawks', colorClass: 'team-seattle' }
    ]
};

// State
let simulationMode = 'historical'; // 'historical', 'this-week', 'hypothetical'
let selectedPlayer = null;
let selectedTeam = null;
let selectedDate = null;
let homeAway = 'vs'; // 'vs' or 'away'

// Mock Data - This Week's Schedule
// TODO: Replace with real API call
const mockThisWeekSchedule = {
    // Bijan Robinson (from user requirements)
    9: { opponent: 'TB', location: 'vs', opponentName: 'Tampa Bay Buccaneers' }, // Using Christian McCaffrey's ID for Bijan
    // Tyreek Hill (from user requirements)
    16: { opponent: 'NYJ', location: '@', opponentName: 'New York Jets' },
    // Add more mock schedules as needed
    1: { opponent: 'DEN', location: 'vs', opponentName: 'Denver Broncos' },
    2: { opponent: 'KC', location: '@', opponentName: 'Kansas City Chiefs' },
    3: { opponent: 'CLE', location: 'vs', opponentName: 'Cleveland Browns' },
    4: { opponent: 'DAL', location: '@', opponentName: 'Dallas Cowboys' },
    5: { opponent: 'PIT', location: 'vs', opponentName: 'Pittsburgh Steelers' },
    6: { opponent: 'PHI', location: 'vs', opponentName: 'Philadelphia Eagles' },
    7: { opponent: 'LV', location: '@', opponentName: 'Las Vegas Raiders' },
    8: { opponent: 'IND', location: 'vs', opponentName: 'Indianapolis Colts' },
    10: { opponent: 'SEA', location: '@', opponentName: 'Seattle Seahawks' },
    11: { opponent: 'KC', location: 'vs', opponentName: 'Kansas City Chiefs' },
    12: { opponent: 'WAS', location: '@', opponentName: 'Washington Commanders' },
    13: { opponent: 'MIA', location: 'vs', opponentName: 'Miami Dolphins' },
    14: { opponent: 'DET', location: '@', opponentName: 'Detroit Lions' },
    15: { opponent: 'GB', location: 'vs', opponentName: 'Green Bay Packers' },
    17: { opponent: 'BUF', location: '@', opponentName: 'Buffalo Bills' },
    18: { opponent: 'NE', location: 'vs', opponentName: 'New England Patriots' },
    19: { opponent: 'SF', location: '@', opponentName: 'San Francisco 49ers' },
    20: { opponent: 'NYJ', location: 'vs', opponentName: 'New York Jets' },
    21: { opponent: 'CHI', location: '@', opponentName: 'Chicago Bears' },
    22: { opponent: 'DAL', location: 'vs', opponentName: 'Dallas Cowboys' },
    23: { opponent: 'DEN', location: '@', opponentName: 'Denver Broncos' },
    24: { opponent: 'CIN', location: 'vs', opponentName: 'Cincinnati Bengals' },
    25: { opponent: 'MIN', location: '@', opponentName: 'Minnesota Vikings' },
    26: { opponent: 'GB', location: 'vs', opponentName: 'Green Bay Packers' },
    27: { opponent: 'LAC', location: '@', opponentName: 'Los Angeles Chargers' },
    28: { opponent: 'KC', location: 'vs', opponentName: 'Kansas City Chiefs' },
    29: { opponent: 'LV', location: '@', opponentName: 'Las Vegas Raiders' },
    30: { opponent: 'BAL', location: 'vs', opponentName: 'Baltimore Ravens' }
};

// Mock Data - Historical Game Dates
// TODO: Replace with real API call
const mockHistoricalDates = {
    // Format: playerId_opponentId: [array of dates]
    '9_TB': ['2023-11-19', '2022-10-23', '2021-09-26'],
    '9_PHI': ['2023-12-03', '2022-11-20'],
    '9_DAL': ['2023-10-08', '2022-09-11'],
    '9_SF': ['2023-01-15', '2022-12-18'],
    '16_NYJ': ['2023-11-12', '2022-10-09', '2021-11-21'],
    '16_BUF': ['2023-12-17', '2022-11-13'],
    '16_NE': ['2023-10-29', '2022-09-18'],
    '16_MIA': ['2023-09-24', '2022-10-30'],
    // Add more combinations as needed - default fallback dates
    'default': ['2023-11-26', '2023-10-15', '2023-09-10', '2022-12-11', '2022-11-06']
};

// Add Bijan Robinson to mock players (user requirement)
// Note: Using ID 31 to avoid conflicts
mockPlayers.push({ id: 31, name: 'Bijan Robinson', position: 'RB', team: 'Atlanta Falcons', initials: 'BR' });
mockThisWeekSchedule[31] = { opponent: 'TB', location: 'vs', opponentName: 'Tampa Bay Buccaneers' };

// Initialize on DOM load
document.addEventListener('DOMContentLoaded', () => {
    initializeModeToggle();
    initializePlayerSearch();
    initializeTeamSelection();
    initializeHomeAwayToggle();
    initializeDateSelection();
    initializeSimulationButton();
    updateCalculateButton();
    switchMode('historical'); // Set initial mode
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
        const filtered = mockPlayers.filter(player => 
            allowedPositions.includes(player.position) &&
            (player.name.toLowerCase().includes(query) ||
            player.position.toLowerCase().includes(query) ||
            player.team.toLowerCase().includes(query))
        );

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

    autocompleteResults.innerHTML = players.map(player => `
        <div class="autocomplete-item" data-player-id="${player.id}">
            <div class="player-headshot">${player.initials}</div>
            <div class="player-info">
                <div class="player-name">${player.name}</div>
                <div class="player-details">${player.position} • ${player.team}</div>
            </div>
        </div>
    `).join('');

    autocompleteResults.classList.add('active');

    // Add click handlers
    autocompleteResults.querySelectorAll('.autocomplete-item').forEach(item => {
        item.addEventListener('click', () => {
            const playerId = parseInt(item.dataset.playerId);
            selectPlayer(playerId);
        });
    });
}

function selectPlayer(playerId) {
    const player = mockPlayers.find(p => p.id === playerId);
    if (!player) return;

    selectedPlayer = player;
    
    const searchInput = document.getElementById('player-search');
    const autocompleteResults = document.getElementById('autocomplete-results');
    const selectedPlayerDiv = document.getElementById('selected-player');

    if (searchInput) searchInput.value = player.name;
    if (autocompleteResults) autocompleteResults.classList.remove('active');
    
    if (selectedPlayerDiv) {
        selectedPlayerDiv.innerHTML = `
            <div style="display: flex; align-items: center; gap: 0.75rem;">
                <div class="player-headshot">${player.initials}</div>
                <div style="flex: 1; min-width: 0;">
                    <div class="player-name">${player.name}</div>
                    <div class="player-details">${player.position} • ${player.team}</div>
                </div>
            </div>
        `;
        selectedPlayerDiv.classList.add('active');
    }
    
    // Mode-specific updates
    if (simulationMode === 'this-week') {
        updateThisWeekMatchup();
    } else if (simulationMode === 'historical') {
        updateHistoricalDates();
    }
    
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
        // Auto-fill opponent and location for this week's game
        updateThisWeekMatchup();
    } else {
        // Clear auto-filled fields
        const autoOpponent = document.getElementById('auto-opponent');
        const autoLocation = document.getElementById('auto-location');
        if (autoOpponent) autoOpponent.textContent = '--';
        if (autoLocation) autoLocation.textContent = '--';
    }
    
    // Re-initialize team selection for the active mode
    initializeTeamSelection();
    
    // If switching to historical mode and player/team already selected, populate dates
    if (mode === 'historical' && selectedPlayer && selectedTeam) {
        updateHistoricalDates();
    }
    
    // Update button state
    updateCalculateButton();
}

function updateThisWeekMatchup() {
    if (!selectedPlayer) return;
    
    const schedule = mockThisWeekSchedule[selectedPlayer.id];
    const autoOpponent = document.getElementById('auto-opponent');
    const autoLocation = document.getElementById('auto-location');
    
    if (schedule && autoOpponent && autoLocation) {
        autoOpponent.textContent = schedule.opponentName;
        autoLocation.textContent = schedule.location === 'vs' ? 'VS (HOME)' : '@ (AWAY)';
        selectedTeam = schedule.opponent;
        homeAway = schedule.location;
    } else {
        if (autoOpponent) autoOpponent.textContent = '--';
        if (autoLocation) autoLocation.textContent = '--';
        selectedTeam = null;
    }
    
    updateCalculateButton();
}

// Team Selection Functionality
function initializeTeamSelection() {
    // Get the appropriate team selection container based on mode
    let teamSelectionDiv;
    if (simulationMode === 'historical') {
        teamSelectionDiv = document.getElementById('team-selection-historical');
    } else if (simulationMode === 'hypothetical') {
        teamSelectionDiv = document.getElementById('team-selection-hypothetical');
    } else {
        // This week's game doesn't need team selection
        return;
    }
    
    if (!teamSelectionDiv) return;

    let html = '';

    // Render teams by division
    const divisions = Object.keys(mockTeams);
    divisions.forEach(division => {
        html += '<div class="division-group">';
        html += `<div class="division-title">${division}</div>`;
        html += '<div class="team-grid">';
        mockTeams[division].forEach(team => {
            html += `
                <div class="team-item ${team.colorClass}" data-team-id="${team.id}">
                    <span class="team-name">${team.name}</span>
                </div>
            `;
        });
        html += '</div></div>';
    });

    teamSelectionDiv.innerHTML = html;

    // Add click handlers
    teamSelectionDiv.querySelectorAll('.team-item').forEach(item => {
        item.addEventListener('click', () => {
            const teamId = item.dataset.teamId;
            selectTeam(teamId);
        });
    });
    
    // Restore selection if exists
    if (selectedTeam) {
        const selectedItem = teamSelectionDiv.querySelector(`[data-team-id="${selectedTeam}"]`);
        if (selectedItem) {
            selectedItem.classList.add('selected');
        }
    }
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
        selectedDate = e.target.value;
        updateCalculateButton();
    });
}

function updateHistoricalDates() {
    if (!selectedPlayer || !selectedTeam) {
        const dateSelect = document.getElementById('game-date-select');
        if (dateSelect) {
            dateSelect.innerHTML = '<option value="">-- SELECT DATE --</option>';
            dateSelect.value = '';
        }
        selectedDate = null;
        return;
    }
    
    // Get historical dates for this player-opponent combination
    const dateKey = `${selectedPlayer.id}_${selectedTeam}`;
    let dates = mockHistoricalDates[dateKey];
    
    // Fallback to default dates if no specific match found
    if (!dates || dates.length === 0) {
        dates = mockHistoricalDates['default'];
    }
    
    const dateSelect = document.getElementById('game-date-select');
    if (!dateSelect) return;
    
    dateSelect.innerHTML = '<option value="">-- SELECT DATE --</option>';
    dates.forEach(date => {
        const option = document.createElement('option');
        option.value = date;
        // Format date for display (e.g., "Nov 19, 2023")
        const dateObj = new Date(date);
        const formattedDate = dateObj.toLocaleDateString('en-US', { 
            year: 'numeric', 
            month: 'short', 
            day: 'numeric' 
        });
        option.textContent = formattedDate;
        dateSelect.appendChild(option);
    });
    
    selectedDate = null;
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

// Update Calculate Button State
function updateCalculateButton() {
    const simulationButton = document.getElementById('simulation-button');
    if (!simulationButton) return;

    let isValid = false;
    
    switch(simulationMode) {
        case 'historical':
            isValid = selectedPlayer && selectedTeam && selectedDate;
            break;
        case 'this-week':
            isValid = selectedPlayer && selectedTeam; // Team is auto-filled
            break;
        case 'hypothetical':
            isValid = selectedPlayer && selectedTeam;
            break;
    }
    
    simulationButton.disabled = !isValid;
}

// Simulation Button
function initializeSimulationButton() {
    const simulationButton = document.getElementById('simulation-button');
    if (!simulationButton) return;

    const handleActivate = () => {
        // Validate based on mode
        let isValid = false;
        switch(simulationMode) {
            case 'historical':
                isValid = selectedPlayer && selectedTeam && selectedDate;
                break;
            case 'this-week':
                isValid = selectedPlayer && selectedTeam;
                break;
            case 'hypothetical':
                isValid = selectedPlayer && selectedTeam;
                break;
        }
        
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
            startTerminalAnimation();
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

function getTeamName(teamId) {
    const allTeams = Object.values(mockTeams).flat();
    const team = allTeams.find(t => t.id === teamId);
    return team ? team.name : 'Unknown Team';
}

// Terminal Animation
function startTerminalAnimation() {
    const terminalOutput = document.getElementById('terminalOutput');
    
    if (!terminalOutput) return;

    // Clear previous output
    terminalOutput.innerHTML = '<div class="terminal-line-wrapper"><span class="terminal-cursor" id="terminalCursor">_</span></div>';
    
    // Get cursor reference after recreating HTML
    const terminalCursor = document.getElementById('terminalCursor');

    const playerName = selectedPlayer.name;
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
        { text: '    Scanning data/processed/player_weekly.parquet', delay: 1200, class: 'info' },
        { text: '    Scanning data/processed/defense_weekly.parquet', delay: 1400, class: 'info' },
        { text: '    Scanning data/processed/game_context.parquet', delay: 1600, class: 'info' },
        { text: '    Loaded 12,847 player-game records (2019-2024)', delay: 1800, class: 'success' },
        { text: '    Loaded 1,536 team-defense records', delay: 2000, class: 'success' },
        { text: '    Loaded 1,536 game context records', delay: 2200, class: 'success' },
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
