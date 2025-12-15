// Recent Predictions Page - Mock Data and Functionality

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

// Generate mock predictions from the last 14 days
function generateMockPredictions() {
    const predictions = [];
    const now = new Date();
    // Only offensive skilled positions: QB, RB, WR, TE, K
    const players = [
        // QBs
        { name: 'Patrick Mahomes', position: 'QB', opponent: 'Kansas City Chiefs' },
        { name: 'Josh Allen', position: 'QB', opponent: 'Buffalo Bills' },
        { name: 'Lamar Jackson', position: 'QB', opponent: 'Baltimore Ravens' },
        { name: 'Jalen Hurts', position: 'QB', opponent: 'Philadelphia Eagles' },
        { name: 'Justin Herbert', position: 'QB', opponent: 'Los Angeles Chargers' },
        { name: 'C.J. Stroud', position: 'QB', opponent: 'Houston Texans' },
        { name: 'Dak Prescott', position: 'QB', opponent: 'Dallas Cowboys' },
        { name: 'Tua Tagovailoa', position: 'QB', opponent: 'Miami Dolphins' },
        { name: 'Brock Purdy', position: 'QB', opponent: 'San Francisco 49ers' },
        { name: 'Jared Goff', position: 'QB', opponent: 'Detroit Lions' },
        // RBs
        { name: 'Christian McCaffrey', position: 'RB', opponent: 'San Francisco 49ers' },
        { name: 'Derrick Henry', position: 'RB', opponent: 'Tennessee Titans' },
        { name: 'Breece Hall', position: 'RB', opponent: 'New York Jets' },
        { name: 'Alvin Kamara', position: 'RB', opponent: 'New Orleans Saints' },
        { name: 'Saquon Barkley', position: 'RB', opponent: 'New York Giants' },
        { name: 'Josh Jacobs', position: 'RB', opponent: 'Green Bay Packers' },
        { name: 'Tony Pollard', position: 'RB', opponent: 'Tennessee Titans' },
        // WRs
        { name: 'Tyreek Hill', position: 'WR', opponent: 'Miami Dolphins' },
        { name: 'CeeDee Lamb', position: 'WR', opponent: 'Dallas Cowboys' },
        { name: 'Amon-Ra St. Brown', position: 'WR', opponent: 'Detroit Lions' },
        { name: 'A.J. Brown', position: 'WR', opponent: 'Philadelphia Eagles' },
        { name: 'Davante Adams', position: 'WR', opponent: 'Las Vegas Raiders' },
        { name: 'Stefon Diggs', position: 'WR', opponent: 'Buffalo Bills' },
        { name: 'Cooper Kupp', position: 'WR', opponent: 'Los Angeles Rams' },
        // TEs
        { name: 'Travis Kelce', position: 'TE', opponent: 'Kansas City Chiefs' },
        { name: 'Mark Andrews', position: 'TE', opponent: 'Baltimore Ravens' },
        { name: 'T.J. Hockenson', position: 'TE', opponent: 'Minnesota Vikings' },
        { name: 'Sam LaPorta', position: 'TE', opponent: 'Detroit Lions' },
        // Kickers
        { name: 'Justin Tucker', position: 'K', opponent: 'Baltimore Ravens' },
        { name: 'Daniel Carlson', position: 'K', opponent: 'Las Vegas Raiders' },
        { name: 'Harrison Butker', position: 'K', opponent: 'Kansas City Chiefs' },
        { name: 'Evan McPherson', position: 'K', opponent: 'Cincinnati Bengals' },
    ];

    // Generate 30-40 random predictions from the last 14 days
    const numPredictions = Math.floor(Math.random() * 11) + 30; // 30-40 predictions
    
    for (let i = 0; i < numPredictions; i++) {
        const player = players[Math.floor(Math.random() * players.length)];
        
        // Random date within last 14 days
        const daysAgo = Math.floor(Math.random() * 14);
        const runDate = new Date(now);
        runDate.setDate(runDate.getDate() - daysAgo);
        runDate.setHours(Math.floor(Math.random() * 24));
        runDate.setMinutes(Math.floor(Math.random() * 60));
        
        // Data snapshot date (1-7 days before run date)
        const snapshotDaysAgo = Math.floor(Math.random() * 7) + 1;
        const snapshotDate = new Date(runDate);
        snapshotDate.setDate(snapshotDate.getDate() - snapshotDaysAgo);
        
        // Generate position-specific stat predictions
        const stats = positionStats[player.position] || ['PPR Fantasy'];
        const statValues = {};
        
        stats.forEach(stat => {
            statValues[stat] = generatePredictedValue(stat);
        });
        
        predictions.push({
            playerName: player.name,
            position: player.position,
            opponent: player.opponent,
            runDate: runDate,
            stats: statValues,
            snapshotDate: snapshotDate
        });
    }
    
    return predictions;
}

// Generate realistic predicted values based on stat type
function generatePredictedValue(statName) {
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
}

// Format date for display
function formatDate(date) {
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    return `${month}/${day}/${year} ${hours}:${minutes}`;
}

// Format date for date input (YYYY-MM-DD)
function formatDateInput(date) {
    const year = date.getFullYear();
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    return `${year}-${month}-${day}`;
}

// Render predictions table
function renderPredictions(predictions, selectedPosition) {
    const tbody = document.getElementById('predictions-tbody');
    const thead = document.getElementById('predictions-thead');
    const noResults = document.getElementById('no-results');
    
    if (predictions.length === 0) {
        tbody.innerHTML = '';
        noResults.style.display = 'block';
        return;
    }
    
    noResults.style.display = 'none';
    
    // Get stats for selected position
    const stats = positionStats[selectedPosition] || [];
    
    // Update table header
    let headerHTML = `
        <tr>
            <th>PLAYER</th>
            <th>POS</th>
            <th>OPPONENT</th>
            <th>RUN DATE</th>
    `;
    stats.forEach(stat => {
        headerHTML += `<th>${stat}</th>`;
    });
    headerHTML += `</tr>`;
    thead.innerHTML = headerHTML;
    
    // Render table rows
    tbody.innerHTML = predictions.map(pred => {
        let rowHTML = `
            <tr>
                <td>${pred.playerName}</td>
                <td>${pred.position}</td>
                <td>${pred.opponent}</td>
                <td>${formatDate(pred.runDate)}</td>
        `;
        stats.forEach(stat => {
            const value = pred.stats[stat] || '--';
            rowHTML += `<td>${value}</td>`;
        });
        rowHTML += `</tr>`;
        return rowHTML;
    }).join('');
}

// Filter predictions
function filterPredictions(allPredictions, position, searchTerm, dateFilter) {
    let filtered = [...allPredictions];
    
    // Filter by position
    if (position) {
        filtered = filtered.filter(pred => pred.position === position);
    }
    
    // Filter by search term (player name)
    if (searchTerm.trim()) {
        const searchLower = searchTerm.toLowerCase().trim();
        filtered = filtered.filter(pred => 
            pred.playerName.toLowerCase().includes(searchLower)
        );
    }
    
    // Filter by date
    if (dateFilter) {
        const filterDate = new Date(dateFilter);
        filterDate.setHours(0, 0, 0, 0);
        filtered = filtered.filter(pred => {
            const predDate = new Date(pred.runDate);
            predDate.setHours(0, 0, 0, 0);
            return predDate.getTime() === filterDate.getTime();
        });
    }
    
    // Sort by date (newest first)
    filtered.sort((a, b) => {
        return b.runDate.getTime() - a.runDate.getTime();
    });
    
    return filtered;
}

// Initialize page
let allPredictions = [];

document.addEventListener('DOMContentLoaded', () => {
    // Generate mock data
    allPredictions = generateMockPredictions();
    
    // Get filter elements
    const positionSelect = document.getElementById('position-select');
    const searchInput = document.getElementById('player-search-input');
    const dateFilter = document.getElementById('date-filter');
    
    // Function to update display
    const updateDisplay = () => {
        const filtered = filterPredictions(
            allPredictions,
            positionSelect.value,
            searchInput.value,
            dateFilter.value
        );
        renderPredictions(filtered, positionSelect.value);
    };
    
    // Initial render (default to QB)
    updateDisplay();
    
    // Add event listeners
    positionSelect.addEventListener('change', updateDisplay);
    searchInput.addEventListener('input', updateDisplay);
    dateFilter.addEventListener('change', updateDisplay);
});

