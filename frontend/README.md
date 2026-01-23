# Frontend - Read the Field

This directory contains the frontend implementation for Read the Field, a probabilistic NFL player simulation demo. The frontend visualizes the prediction process and results through a retro cartridge football aesthetic.

## Localhost Testing

The frontend is a static HTML/CSS/JavaScript application that can be run locally without any build process. It expects the backend API to be running on `http://localhost:8000`.
You can override the API base with `window.RTF_API_URL` if needed.

### Quick Start

**Option 1: Python HTTP Server**
```bash
# Python 3
cd frontend
python3 -m http.server 5173
```

Then open `http://localhost:5173` in your browser.

**Option 2: Node.js HTTP Server**
```bash
# Install http-server globally (if not already installed)
npm install -g http-server

# Run server
cd frontend
http-server -p 5173
```

Then open `http://localhost:5173` in your browser.

**Option 3: VS Code Live Server**
If using VS Code, install the "Live Server" extension and right-click on `index.html` to select "Open with Live Server".

### File Structure

- `index.html` - Home page with player and team selection
- `calculations.html` - Terminal-style calculations visualization
- `about.html` - About page
- `contact.html` - Contact page
- `styles.css` - Main stylesheet (Cartridge Football theme)
- `design-system.css` - Design system utilities
- `app.js` - Main application logic
- `calculations.js` - Terminal animation logic

### Design System

The frontend follows a strict Cartridge Football aesthetic:
- Pixel-style monospace font (Press Start 2P)
- Flat colors only (no gradients)
- 3px borders
- 0px border radius
- No hover lift animations
- Inset shadows for pressed states only
- Uppercase button text

### Notes

- Calls `GET /players`, `GET /teams`, `GET /seasons`, `GET /player/:id/games`, `GET /player/:id/next_game`, and `POST /simulate`
- Expects API responses in the `{ ok:true, data:{...} }` contract
- Requires the backend running at `http://localhost:8000`
- All pages are static HTML
- Navigation is handled via footer links
- Team tiles are grouped by conference/division when available
- Opponent tiles are greyed out when the player has never faced that team
- Mode order: Upcoming Game → Hypothetical Matchup → Historical Matchup

### Troubleshooting
- If players/teams do not load, confirm `Rscript api/run_api.R` is running and `http://localhost:8000/players` returns ok:true.
- If simulations fail, check browser console for backend `error_code` and `message`.
- If nickname search is missing, verify `frontend/data/nicknames.json` exists.

### Local Test Checklist
1. `Rscript api/run_api.R`
2. `curl -s http://localhost:8000/players | jq '.ok, (.data.players|length)'`
3. `cd frontend && python3 -m http.server 5173`
4. Open `http://localhost:5173` and verify players load and simulation results render.
