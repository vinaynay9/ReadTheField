# Frontend - Read the Field

This directory contains the frontend implementation for the Read the Field analytics visualization system.

## Localhost Testing

The frontend is a static HTML/CSS/JavaScript application that can be run locally without any build process or backend dependencies.

### Quick Start

**Option 1: Python HTTP Server**
```bash
# Python 3
cd frontend
python -m http.server 8000

# Python 2
python -m SimpleHTTPServer 8000
```

Then open `http://localhost:8000` in your browser.

**Option 2: Node.js HTTP Server**
```bash
# Install http-server globally (if not already installed)
npm install -g http-server

# Run server
cd frontend
http-server -p 8000
```

Then open `http://localhost:8000` in your browser.

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

- No API calls are made - all data is mock data
- No backend required
- All pages are static HTML
- Navigation is handled via footer links
