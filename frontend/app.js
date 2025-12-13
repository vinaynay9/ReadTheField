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
let selectedPlayer = null;
let selectedTeam = null;
let homeAway = 'vs'; // 'vs' or 'away'

// Initialize on DOM load
document.addEventListener('DOMContentLoaded', () => {
    initializePlayerSearch();
    initializeTeamSelection();
    initializeHomeAwayToggle();
    initializeSimulationButton();
    updateCalculateButton();
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
    
    updateCalculateButton();
}

// Team Selection Functionality
function initializeTeamSelection() {
    const teamSelectionDiv = document.getElementById('team-selection');
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
}

function selectTeam(teamId) {
    // Remove previous selection
    document.querySelectorAll('.team-item').forEach(item => {
        item.classList.remove('selected');
    });

    // Add selection to clicked team
    const selectedItem = document.querySelector(`[data-team-id="${teamId}"]`);
    if (selectedItem) {
        selectedItem.classList.add('selected');
        selectedTeam = teamId;
        updateCalculateButton();
    }
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
    });

    toggleAway.addEventListener('click', () => {
        homeAway = 'away';
        toggleAway.classList.add('active');
        toggleVs.classList.remove('active');
    });
}

// Update Calculate Button State
function updateCalculateButton() {
    const simulationButton = document.getElementById('simulation-button');
    if (!simulationButton) return;

    if (selectedPlayer && selectedTeam) {
        simulationButton.disabled = false;
    } else {
        simulationButton.disabled = true;
    }
}

// Simulation Button
function initializeSimulationButton() {
    const simulationButton = document.getElementById('simulation-button');
    if (!simulationButton) return;

    const handleActivate = () => {
        if (!selectedPlayer || !selectedTeam) {
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
        { text: '    Game context: Week 14, 2024', delay: 3200, class: 'info' },
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
            
            // Generate mock predicted values for each stat
            const generatePredictedValue = (statName) => {
                // Generate realistic mock values based on stat type
                if (statName.includes('YD') || statName === 'PYD' || statName === 'RYD' || statName === 'RECYD' || statName === 'RUYD') {
                    return Math.floor(Math.random() * 150) + 20; // Yards: 20-170
                } else if (statName.includes('TD') || statName === 'PTD' || statName === 'RTD' || statName === 'RECTD' || statName === 'TOTTD') {
                    return Math.floor(Math.random() * 3); // Touchdowns: 0-2
                } else if (statName === 'Receptions') {
                    return Math.floor(Math.random() * 10) + 2; // Receptions: 2-11
                } else if (statName === 'Carries' || statName === 'P Attempts' || statName === 'R Attempts') {
                    return Math.floor(Math.random() * 25) + 5; // Attempts: 5-29
                } else if (statName === 'Completions') {
                    return Math.floor(Math.random() * 20) + 10; // Completions: 10-29
                } else if (statName === 'Tackles') {
                    return Math.floor(Math.random() * 12) + 3; // Tackles: 3-14
                } else if (statName === 'Sacks') {
                    return Math.floor(Math.random() * 2); // Sacks: 0-1
                } else if (statName === 'FF') {
                    return Math.floor(Math.random() * 2); // Forced Fumbles: 0-1
                } else if (statName === 'Interceptions') {
                    return Math.floor(Math.random() * 2); // Interceptions: 0-1
                } else if (statName === 'Passes Defended') {
                    return Math.floor(Math.random() * 4); // Passes Defended: 0-3
                } else if (statName === 'FG Made') {
                    return Math.floor(Math.random() * 3) + 1; // FG Made: 1-3
                } else if (statName === 'FG Attempts') {
                    return Math.floor(Math.random() * 3) + 2; // FG Attempts: 2-4
                } else if (statName === '# PAT') {
                    return Math.floor(Math.random() * 5) + 2; // PAT: 2-6
                } else if (statName === '# Punts') {
                    return Math.floor(Math.random() * 6) + 2; // Punts: 2-7
                } else if (statName === 'Punts within 10/20 yards') {
                    return Math.floor(Math.random() * 3); // Punts within 10/20: 0-2
                } else {
                    return Math.floor(Math.random() * 20) + 5; // Default: 5-24
                }
            };
            
            // Create result items for each stat
            stats.forEach(stat => {
                const predictedValue = generatePredictedValue(stat);
                const resultItem = document.createElement('div');
                resultItem.className = 'result-item';
                resultItem.innerHTML = `
                    <div class="result-label">${stat}</div>
                    <div class="result-value">${predictedValue}</div>
                `;
                resultsContent.appendChild(resultItem);
            });
            
            // If no stats for position, show default message
            if (stats.length === 0) {
                resultsContent.innerHTML = `
                    <div class="result-item" style="grid-column: 1 / -1;">
                        <div class="result-label">NO STATS AVAILABLE</div>
                        <div class="result-value">--</div>
                    </div>
                `;
            }
        }
    }

    runAnimation();
}
