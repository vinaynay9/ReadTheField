// Terminal Animation for Calculations Page
// Simulates the Read the Field modeling process as retro terminal output

(function() {
    'use strict';

    const terminalOutput = document.getElementById('terminalOutput');
    let terminalCursor = document.getElementById('terminalCursor');
    
    if (!terminalOutput) return;

    // Terminal output lines with timing and styling
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
        { text: '    Loaded 12,847 player-game records (2019-2024)', delay: 1800, class: 'success' },
        { text: '    Loaded 1,536 team-defense records', delay: 2000, class: 'success' },
        { text: '    Loaded 1,536 game context records', delay: 2200, class: 'success' },
        { text: '', delay: 2400, class: 'info' },
        { text: '[2/7] Identifying target player and matchup...', delay: 2600, class: 'info' },
        { text: '    Player: CMC (RB, San Francisco 49ers)', delay: 2800, class: 'info' },
        { text: '    Opponent: Seattle Seahawks (Away)', delay: 3000, class: 'info' },
        { text: '    Game context: Week 14, 2024', delay: 3200, class: 'info' },
        { text: '', delay: 3400, class: 'info' },
        { text: '[3/7] Building feature set...', delay: 3600, class: 'info' },
        { text: '    Computing recent usage metrics (last 4 games)', delay: 3800, class: 'info' },
        { text: '    Computing matchup features (opponent defense vs RB)', delay: 4000, class: 'info' },
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
        { text: '    Number of trees: 450', delay: 6800, class: 'info' },
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
        { text: '    Mean projected yards: 87.4', delay: 9800, class: 'success' },
        { text: '', delay: 10000, class: 'info' },
        { text: 'PERCENTILE OUTCOMES', delay: 10200, class: 'highlight' },
        { text: '==================', delay: 10400, class: 'info' },
        { text: '    P10: 52.3 yards', delay: 10600, class: 'info' },
        { text: '    P50: 84.1 yards', delay: 10800, class: 'info' },
        { text: '    P90: 128.7 yards', delay: 11000, class: 'info' },
        { text: '', delay: 11200, class: 'info' },
        { text: '', delay: 11400, class: 'info' },
        { text: '> Simulation complete.', delay: 11600, class: 'prompt' },
        { text: '', delay: 11800, class: 'info' },
        { text: 'System ready for next simulation.', delay: 12000, class: 'success' },
        { text: '', delay: 13400, class: 'info' }
    ];

    let currentLineIndex = 0;
    let isTyping = false;
    let animationId = null;
    const typingSpeed = 10; // milliseconds per character (fast and lightweight)
    const lineDelay = 30; // milliseconds between lines

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
        isTyping = true;

        function typeChar() {
            if (charIndex < text.length) {
                element.textContent += text[charIndex];
                charIndex++;
                animationId = setTimeout(typeChar, typingSpeed);
            } else {
                isTyping = false;
                callback();
            }
        }

        typeChar();
    }

    function addLine(lineData) {
        return new Promise((resolve) => {
            const lineElement = createLineElement(lineData);
            // Insert before the cursor wrapper
            const cursorWrapper = terminalOutput.querySelector('.terminal-line-wrapper');
            if (cursorWrapper && cursorWrapper.parentNode === terminalOutput) {
                terminalOutput.insertBefore(lineElement, cursorWrapper);
            } else {
                terminalOutput.appendChild(lineElement);
            }
            
            // Scroll to bottom after a short delay to ensure content is rendered
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
        terminalOutput.innerHTML = '<div class="terminal-line-wrapper"><span class="terminal-cursor" id="terminalCursor">_</span></div>';
        terminalCursor = document.getElementById('terminalCursor');
        currentLineIndex = 0;
        if (terminalCursor) {
            terminalCursor.style.display = 'inline-block';
        }

        let lastDelay = 0;
        for (let i = 0; i < terminalLines.length; i++) {
            const lineData = terminalLines[i];
            
            // Wait for the relative delay before starting this line
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

        // Wait a bit before restarting
        setTimeout(() => {
            runAnimation();
        }, 3000);
    }

    // Start animation when page loads
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', runAnimation);
    } else {
        runAnimation();
    }

    // Clean up on page unload
    window.addEventListener('beforeunload', () => {
        if (animationId) {
            clearTimeout(animationId);
        }
    });
})();
