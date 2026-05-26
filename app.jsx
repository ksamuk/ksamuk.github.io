/* eslint-disable */
/* Samuk Lab — Routing + Tweaks app shell */

const { useState: useAppState, useEffect: useAppEffect, useMemo: useAppMemo } = React;

const TWEAK_DEFAULTS = /*EDITMODE-BEGIN*/{
  "accent": "#28bbf8",
  "heroOverlay": "soft",
  "kickerStyle": "rule",
  "showScrollHint": true,
  "newsAutoplaySec": 7,
  "rotateHeroHeadline": true
}/*EDITMODE-END*/;

function App() {
  const [t, setTweak] = useTweaks(TWEAK_DEFAULTS);

  const VALID_PAGES = ['home', 'research', 'people', 'publications', 'join', 'software', 'contact'];

  // hash-based routing
  const [page, setPage] = useAppState(() => {
    const h = (window.location.hash || '').replace('#', '');
    return h && VALID_PAGES.includes(h) ? h : 'home';
  });

  useAppEffect(() => {
    const onHash = () => {
      const h = (window.location.hash || '').replace('#', '');
      const next = h && VALID_PAGES.includes(h) ? h : 'home';
      setPage(next);
      window.scrollTo({ top: 0, behavior: 'instant' in window ? 'instant' : 'auto' });
    };
    window.addEventListener('hashchange', onHash);
    return () => window.removeEventListener('hashchange', onHash);
  }, []);

  const navigate = (id) => {
    if (id === page) {
      window.scrollTo({ top: 0, behavior: 'smooth' });
      return;
    }
    window.location.hash = id;
  };

  // Apply tweaks to CSS vars
  useAppEffect(() => {
    const root = document.documentElement;
    root.style.setProperty('--sl-cyan', t.accent);
    root.style.setProperty('--sl-cyan-hover', shade(t.accent, -0.32));
    root.style.setProperty('--sl-cyan-soft', shade(t.accent, 0.85));
  }, [t.accent]);

  useAppEffect(() => {
    document.body.dataset.heroOverlay = t.heroOverlay;
    document.body.dataset.kickerStyle = t.kickerStyle;
    document.body.dataset.scrollHint = t.showScrollHint ? 'on' : 'off';
  }, [t.heroOverlay, t.kickerStyle, t.showScrollHint]);

  const PAGES = {
    home:         (props) => <HomePage {...props} />,
    research:     (props) => <ResearchPage {...props} />,
    people:       (props) => <PeoplePage {...props} />,
    publications: (props) => <PublicationsPage {...props} />,
    join:         (props) => <JoinPage {...props} />,
    software:     (props) => <SoftwarePage {...props} />,
    contact:      (props) => <ContactPage {...props} />,
  };

  const PageComp = PAGES[page] || PAGES.home;

  // Pass tweaks where they affect a child component
  const propsFor = (id) => {
    const base = { onNavigate: navigate, tweaks: t };
    if (id === 'home') {
      base.newsAutoMs = t.newsAutoplaySec * 1000;
      base.retypeEnabled = t.rotateHeroHeadline;
    }
    return base;
  };

  return (
    <>
      <Header active={page} onNavigate={navigate} />
      <PageComp {...propsFor(page)} />

      <TweaksPanel>
        <TweakSection label="Brand" />
        <TweakColor
          label="Accent"
          value={t.accent}
          options={['#28bbf8', '#0ea5e9', '#ef6c4a', '#10b981', '#7c5cff', '#1a2532']}
          onChange={(v) => setTweak('accent', v)}
        />

        <TweakSection label="Hero" />
        <TweakRadio
          label="Overlay"
          value={t.heroOverlay}
          options={['soft', 'deep', 'tint']}
          onChange={(v) => setTweak('heroOverlay', v)}
        />
        <TweakRadio
          label="Kicker"
          value={t.kickerStyle}
          options={['rule', 'pill', 'none']}
          onChange={(v) => setTweak('kickerStyle', v)}
        />
        <TweakToggle
          label="Rotating headline"
          value={t.rotateHeroHeadline}
          onChange={(v) => setTweak('rotateHeroHeadline', v)}
        />
        <TweakToggle
          label="Scroll hint"
          value={t.showScrollHint}
          onChange={(v) => setTweak('showScrollHint', v)}
        />

        <TweakSection label="Home" />
        <TweakSlider
          label="News autoplay"
          value={t.newsAutoplaySec}
          min={0} max={15} step={1}
          unit="s"
          onChange={(v) => setTweak('newsAutoplaySec', v)}
        />
      </TweaksPanel>
    </>
  );
}

// Lighten or darken a hex by amt (-1..1)
function shade(hex, amt) {
  const h = hex.replace('#', '');
  const num = parseInt(h.length === 3 ? h.split('').map(c => c + c).join('') : h, 16);
  const r = (num >> 16) & 0xff;
  const g = (num >> 8) & 0xff;
  const b = num & 0xff;
  const adj = (c) => amt >= 0
    ? Math.round(c + (255 - c) * amt)
    : Math.round(c * (1 + amt));
  const toHex = n => Math.max(0, Math.min(255, n)).toString(16).padStart(2, '0');
  return `#${toHex(adj(r))}${toHex(adj(g))}${toHex(adj(b))}`;
}

ReactDOM.createRoot(document.getElementById('root')).render(<App />);
