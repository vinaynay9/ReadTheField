const RUNS_KEY = 'rtf_runs_v1';

const positionColumns = {
    QB: [
        { key: 'passing_yards', label: 'PASS YDS' },
        { key: 'passing_tds', label: 'PASS TD' },
        { key: 'rush_yards', label: 'RUSH YDS' },
        { key: 'rush_tds', label: 'RUSH TD' },
        { key: 'pass_attempts', label: 'PASS ATT' },
        { key: 'completions', label: 'COMP' },
        { key: 'rush_attempts', label: 'RUSH ATT' },
        { key: 'fantasy_points', label: 'FANTASY' }
    ],
    RB: [
        { key: 'rushing_yards', label: 'RUSH YDS' },
        { key: 'receiving_yards', label: 'REC YDS' },
        { key: 'total_touchdowns', label: 'TOTAL TD' },
        { key: 'targets', label: 'TGT' },
        { key: 'receptions', label: 'REC' },
        { key: 'carries', label: 'CAR' },
        { key: 'fantasy_points', label: 'FANTASY' }
    ],
    WR: [
        { key: 'receiving_yards', label: 'REC YDS' },
        { key: 'total_touchdowns', label: 'TOTAL TD' },
        { key: 'targets', label: 'TGT' },
        { key: 'receptions', label: 'REC' },
        { key: 'fantasy_points', label: 'FANTASY' }
    ],
    TE: [
        { key: 'receiving_yards', label: 'REC YDS' },
        { key: 'total_touchdowns', label: 'TOTAL TD' },
        { key: 'targets', label: 'TGT' },
        { key: 'receptions', label: 'REC' },
        { key: 'fantasy_points', label: 'FANTASY' }
    ],
    K: [
        { key: 'fg_made', label: 'FG MADE' },
        { key: 'fg_attempts', label: 'FG ATT' },
        { key: 'xp_made', label: 'XP MADE' },
        { key: 'xp_attempts', label: 'XP ATT' },
        { key: 'fantasy_points', label: 'FANTASY' }
    ]
};

function loadRuns() {
    try {
        return JSON.parse(localStorage.getItem(RUNS_KEY) || '[]');
    } catch (err) {
        return [];
    }
}

function formatRunDate(value) {
    if (!value) return '--';
    const dt = new Date(value);
    if (Number.isNaN(dt.getTime())) return '--';
    return dt.toLocaleString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit'
    });
}

function renderTable(runs, position) {
    const tbody = document.getElementById('predictions-tbody');
    const thead = document.getElementById('predictions-thead');
    const noResults = document.getElementById('no-results');
    if (!tbody || !thead || !noResults) return;

    if (!runs || runs.length === 0) {
        tbody.innerHTML = '';
        noResults.style.display = 'block';
        noResults.textContent = 'No successful runs yet. Run a simulation to see it here.';
        return;
    }

    noResults.style.display = 'none';
    const columns = positionColumns[position] || [];
    let headerHTML = `<tr>
        <th>PLAYER</th>
        <th>POS</th>
        <th>OPPONENT</th>
        <th>RUN DATE</th>`;
    columns.forEach(col => {
        headerHTML += `<th>${col.label}</th>`;
    });
    headerHTML += `<th>ACTION</th></tr>`;
    thead.innerHTML = headerHTML;

    tbody.innerHTML = runs.map(run => {
        let rowHTML = `<tr>
            <td>${run.player_name || '--'}</td>
            <td>${run.position || '--'}</td>
            <td>${run.opponent || '--'}</td>
            <td>${formatRunDate(run.run_date)}</td>`;
        columns.forEach(col => {
            const value = run.stats && typeof run.stats[col.key] !== 'undefined' && run.stats[col.key] !== null
                ? Math.round(run.stats[col.key] * 100) / 100
                : '--';
            rowHTML += `<td>${value}</td>`;
        });
        rowHTML += `<td><a class="footer-link" href="index.html?run_id=${run.run_id}">View Report</a></td></tr>`;
        return rowHTML;
    }).join('');
}

function applyFilters(runs) {
    const positionSelect = document.getElementById('position-select');
    const searchInput = document.getElementById('player-search-input');
    const dateInput = document.getElementById('date-filter');

    const position = positionSelect ? positionSelect.value : '';
    const search = searchInput ? searchInput.value.trim().toLowerCase() : '';
    const dateFilter = dateInput ? dateInput.value : '';

    let filtered = runs.filter(run => {
        if (position && run.position !== position) return false;
        if (search && !run.player_name.toLowerCase().includes(search)) return false;
        if (dateFilter) {
            const runDate = new Date(run.run_date);
            const filterDate = new Date(dateFilter);
            if (Number.isNaN(runDate.getTime())) return false;
            if (runDate.toDateString() !== filterDate.toDateString()) return false;
        }
        return true;
    });

    renderTable(filtered, position || 'QB');
}

document.addEventListener('DOMContentLoaded', () => {
    const runs = loadRuns();
    renderTable(runs, 'QB');

    const positionSelect = document.getElementById('position-select');
    const searchInput = document.getElementById('player-search-input');
    const dateInput = document.getElementById('date-filter');

    if (positionSelect) {
        positionSelect.addEventListener('change', () => applyFilters(runs));
    }
    if (searchInput) {
        searchInput.addEventListener('input', () => applyFilters(runs));
    }
    if (dateInput) {
        dateInput.addEventListener('change', () => applyFilters(runs));
    }
});
