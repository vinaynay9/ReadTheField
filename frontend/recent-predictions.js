// Recent Predictions Page - Mock Data and Functionality

// Generate mock predictions from the last 14 days
function generateMockPredictions() {
    const predictions = [];
    const now = new Date();
    const players = [
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
        { name: 'Travis Kelce', position: 'TE', opponent: 'Kansas City Chiefs' },
        { name: 'Mark Andrews', position: 'TE', opponent: 'Baltimore Ravens' },
        { name: 'T.J. Hockenson', position: 'TE', opponent: 'Minnesota Vikings' },
        { name: 'Sam LaPorta', position: 'TE', opponent: 'Detroit Lions' },
        { name: 'Tyreek Hill', position: 'WR', opponent: 'Miami Dolphins' },
        { name: 'CeeDee Lamb', position: 'WR', opponent: 'Dallas Cowboys' },
        { name: 'Amon-Ra St. Brown', position: 'WR', opponent: 'Detroit Lions' },
        { name: 'A.J. Brown', position: 'WR', opponent: 'Philadelphia Eagles' },
        { name: 'Davante Adams', position: 'WR', opponent: 'Las Vegas Raiders' },
        { name: 'Stefon Diggs', position: 'WR', opponent: 'Buffalo Bills' },
        { name: 'Christian McCaffrey', position: 'RB', opponent: 'San Francisco 49ers' },
        { name: 'Derrick Henry', position: 'RB', opponent: 'Tennessee Titans' },
        { name: 'Breece Hall', position: 'RB', opponent: 'New York Jets' },
        { name: 'Alvin Kamara', position: 'RB', opponent: 'New Orleans Saints' },
        { name: 'Saquon Barkley', position: 'RB', opponent: 'New York Giants' },
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
        
        // Generate realistic projection values (for fantasy points)
        const mean = Math.floor(Math.random() * 30) + 10; // 10-40 points
        const p10 = Math.floor(mean * 0.6); // 60% of mean
        const p50 = Math.floor(mean * 0.85); // 85% of mean
        const p90 = Math.floor(mean * 1.4); // 140% of mean
        
        predictions.push({
            playerName: player.name,
            position: player.position,
            opponent: player.opponent,
            runDate: runDate,
            mean: mean,
            p10: p10,
            p50: p50,
            p90: p90,
            snapshotDate: snapshotDate
        });
    }
    
    return predictions;
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
function renderPredictions(predictions) {
    const tbody = document.getElementById('predictions-tbody');
    const noResults = document.getElementById('no-results');
    
    if (predictions.length === 0) {
        tbody.innerHTML = '';
        noResults.style.display = 'block';
        return;
    }
    
    noResults.style.display = 'none';
    tbody.innerHTML = predictions.map(pred => `
        <tr>
            <td>${pred.playerName}</td>
            <td>${pred.position}</td>
            <td>${pred.opponent}</td>
            <td>${formatDate(pred.runDate)}</td>
            <td>${pred.mean}</td>
            <td>${pred.p10}</td>
            <td>${pred.p50}</td>
            <td>${pred.p90}</td>
            <td>${formatDateInput(pred.snapshotDate)}</td>
        </tr>
    `).join('');
}

// Filter and sort predictions
function filterAndSortPredictions(allPredictions, searchTerm, dateFilter, sortBy) {
    let filtered = [...allPredictions];
    
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
    
    // Sort
    const [sortField, sortDirection] = sortBy.split('-');
    filtered.sort((a, b) => {
        let aVal, bVal;
        
        switch(sortField) {
            case 'date':
                aVal = a.runDate.getTime();
                bVal = b.runDate.getTime();
                break;
            case 'mean':
                aVal = a.mean;
                bVal = b.mean;
                break;
            case 'p10':
                aVal = a.p10;
                bVal = b.p10;
                break;
            case 'p50':
                aVal = a.p50;
                bVal = b.p50;
                break;
            case 'p90':
                aVal = a.p90;
                bVal = b.p90;
                break;
            default:
                return 0;
        }
        
        if (sortDirection === 'desc') {
            return bVal - aVal;
        } else {
            return aVal - bVal;
        }
    });
    
    return filtered;
}

// Initialize page
let allPredictions = [];

document.addEventListener('DOMContentLoaded', () => {
    // Generate mock data
    allPredictions = generateMockPredictions();
    
    // Get filter elements
    const searchInput = document.getElementById('player-search-input');
    const dateFilter = document.getElementById('date-filter');
    const sortSelect = document.getElementById('sort-select');
    
    // Initial render
    renderPredictions(allPredictions);
    
    // Add event listeners
    searchInput.addEventListener('input', () => {
        const filtered = filterAndSortPredictions(
            allPredictions,
            searchInput.value,
            dateFilter.value,
            sortSelect.value
        );
        renderPredictions(filtered);
    });
    
    dateFilter.addEventListener('change', () => {
        const filtered = filterAndSortPredictions(
            allPredictions,
            searchInput.value,
            dateFilter.value,
            sortSelect.value
        );
        renderPredictions(filtered);
    });
    
    sortSelect.addEventListener('change', () => {
        const filtered = filterAndSortPredictions(
            allPredictions,
            searchInput.value,
            dateFilter.value,
            sortSelect.value
        );
        renderPredictions(filtered);
    });
});

