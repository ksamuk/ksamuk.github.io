/* eslint-disable */
/* Samuk Lab — Refined components */

const { useState, useEffect, useRef, useMemo } = React;

// ----------------------------------------------------------------
// HEADER
// ----------------------------------------------------------------
function Header({ active = "home", onNavigate }) {
  const [scrolled, setScrolled] = useState(false);
  const [mobileOpen, setMobileOpen] = useState(false);

  useEffect(() => {
    const root = document.scrollingElement || document.documentElement;
    const onScroll = () => setScrolled(root.scrollTop > 60);
    onScroll();
    document.addEventListener('scroll', onScroll, { passive: true });
    return () => document.removeEventListener('scroll', onScroll);
  }, []);

  useEffect(() => {setMobileOpen(false);}, [active]);

  const items = [
  { id: 'home', label: 'Home' },
  { id: 'research', label: 'Research' },
  { id: 'people', label: 'People' },
  { id: 'publications', label: 'Publications' },
  { id: 'software', label: 'Software' },
  { id: 'join', label: 'Join' },
  { id: 'contact', label: 'Contact' }];


  const go = (id) => (e) => {e.preventDefault();onNavigate && onNavigate(id);};

  return (
    <>
      <header className={`site-header${scrolled ? ' scrolled' : ''}`}>
        <div className="container">
          <a className="site-logo" href="#home" onClick={go('home')}>samuk lab</a>
          <ul className="site-menu">
            {items.map((it) =>
            <li key={it.id} className={active === it.id ? 'active' : ''}>
                <a href={`#${it.id}`} onClick={go(it.id)}>{it.label}</a>
              </li>
            )}
          </ul>
          <button className="menu-toggle" aria-label="Menu" onClick={() => setMobileOpen(true)}>
            <i className="fas fa-bars"></i>
          </button>
        </div>
      </header>

      <div className={`mobile-backdrop${mobileOpen ? ' open' : ''}`} onClick={() => setMobileOpen(false)}></div>
      <nav className={`mobile-menu${mobileOpen ? ' open' : ''}`}>
        <button className="close" aria-label="Close" onClick={() => setMobileOpen(false)}>
          <i className="fas fa-times"></i>
        </button>
        <ul>
          {items.map((it) =>
          <li key={it.id} className={active === it.id ? 'active' : ''}>
              <a href={`#${it.id}`} onClick={go(it.id)}>{it.label}</a>
            </li>
          )}
        </ul>
      </nav>
    </>);

}

// ----------------------------------------------------------------
// HERO — supports retype, kicker, inner variant
// ----------------------------------------------------------------
function useRetype(words, enabled) {
  const [text, setText] = useState(words && words.length ? words[0] : '');
  const idxRef = useRef(0);
  const timerRef = useRef(null);

  useEffect(() => {
    if (!enabled || !words || words.length < 2) return;
    const clear = () => {if (timerRef.current) clearTimeout(timerRef.current);};
    let alive = true;
    let current = words[0];
    let i = current.length;
    setText(current);
    idxRef.current = 0;
    const tick = (fn, ms) => {timerRef.current = setTimeout(() => {if (alive) fn();}, ms);};
    const pauseThenDelete = () => tick(deleteOne, 2200);
    const deleteOne = () => {
      if (i > 0) {i--;setText(current.slice(0, i));tick(deleteOne, 60);} else
      {idxRef.current = (idxRef.current + 1) % words.length;
        current = words[idxRef.current];typeOne();}
    };
    const typeOne = () => {
      if (i < current.length) {i++;setText(current.slice(0, i));
        tick(typeOne, 90 + Math.round(Math.random() * 80));} else
      {pauseThenDelete();}
    };
    pauseThenDelete();
    return () => {alive = false;clear();};
  }, [enabled, words ? words.join('|') : '']);

  return text;
}

function Hero({ image, kicker, title, retypeWords, inner = false, innerSub }) {
  const hasRetype = !!(retypeWords && retypeWords.length);
  const word = useRetype(retypeWords, hasRetype);
  return (
    <aside className={`hero${inner ? ' inner' : ''}`} style={{ backgroundImage: `url(${image})` }}>
      <div className="container">
        {inner ?
        <>
            <h1 className="hero-inner-title">{title}</h1>
            {innerSub && <div className="hero-inner-sub">{innerSub}</div>}
          </> :

        <>
            {kicker && <div className="hero-kicker">{kicker}</div>}
            <h1 className="hero-title">
              {title}
              {hasRetype ?
            <>
                  <br />
                  <span className="retype-line" style={{ height: "36px" }}><span className="retype">{word}</span></span>
                </> :
            null}
            </h1>
          </>
        }
      </div>
      {!inner &&
      <div className="hero-scroll-hint">
          <span>Scroll</span>
          <span className="line"></span>
        </div>
      }
    </aside>);

}

// ----------------------------------------------------------------
// FEATURE TRIO
// ----------------------------------------------------------------
function FeatureTrio({ items, onAction }) {
  return (
    <div className="feature-trio">
      {items.map((it, i) =>
      <div className="feature-item" key={i}>
          <div className="feature-icon-wrap">
            <img className="feature-icon" src={it.icon} alt="" />
          </div>
          <h5 className="feature-title">{it.title}</h5>
          <p className="feature-desc" dangerouslySetInnerHTML={{ __html: it.desc }} />
          <a className="btn btn-primary" href="#"
        onClick={(e) => {e.preventDefault();onAction && onAction(it.cta.target);}}>
            {it.cta.label}
          </a>
        </div>
      )}
    </div>);

}

// ----------------------------------------------------------------
// NEWS CAROUSEL
// ----------------------------------------------------------------
function NewsCarousel({ items, autoMs = 7000 }) {
  const [i, setI] = useState(0);
  useEffect(() => {
    if (!autoMs) return;
    const t = setInterval(() => setI((x) => (x + 1) % items.length), autoMs);
    return () => clearInterval(t);
  }, [autoMs, items.length]);

  const it = items[i];
  const prev = () => setI((i - 1 + items.length) % items.length);
  const next = () => setI((i + 1) % items.length);
  return (
    <div className="news-section">
      <div className="container">
        <div className="news-grid" key={i}>
          <div className="news-img-wrap">
            <img className="news-img" src={it.image} alt={it.title} />
            <div className="news-img-tag">{it.tag || 'News'}</div>
          </div>
          <div className="news-text">
            <div className="news-date">{it.date}</div>
            <h3 className="news-title" dangerouslySetInnerHTML={{ __html: it.title }} />
            <p className="news-body">{it.body}</p>
          </div>
        </div>
        <div className="news-controls">
          <button className="news-arrow" aria-label="Previous" onClick={prev}>
            <i className="fas fa-arrow-left"></i>
          </button>
          <div className="news-dots">
            {items.map((_, idx) =>
            <button key={idx} className={`news-dot${i === idx ? ' active' : ''}`}
            aria-label={`News ${idx + 1}`} onClick={() => setI(idx)} />
            )}
          </div>
          <button className="news-arrow" aria-label="Next" onClick={next}>
            <i className="fas fa-arrow-right"></i>
          </button>
        </div>
      </div>
    </div>);

}

// ----------------------------------------------------------------
// RESEARCH CARD
// ----------------------------------------------------------------
function ResearchCard({ image, caption, title, intro, bullets, imageRight = false, number }) {
  const fig =
  <figure className="research-figure">
      <img className="research-img" src={image} alt="" />
      <figcaption className="research-figcaption">{caption}</figcaption>
    </figure>;

  const body =
  <div className="research-body">
      {number && <span className="research-card-number">{number}</span>}
      <h3 className="research-title" dangerouslySetInnerHTML={{ __html: title }} />
      <p dangerouslySetInnerHTML={{ __html: intro }} />
      <p>We have several active projects aimed at understanding this:</p>
      <ul className="research-list">
        {bullets.map((b, i) => <li key={i}>{b}</li>)}
      </ul>
    </div>;

  return (
    <div className={`research-card ${imageRight ? 'right-image' : 'left-image'}`}>
      {imageRight ? <>{body}{fig}</> : <>{fig}{body}</>}
    </div>);

}

// ----------------------------------------------------------------
// SOCIAL ICON & PROFILE CARD
// ----------------------------------------------------------------
function SocialIcon({ kind, href, title }) {
  const ICONS = {
    email: 'fas fa-envelope',
    cv: 'ai ai-cv',
    scholar: 'ai ai-google-scholar',
    orcid: 'ai ai-orcid',
    twitter: 'fab fa-twitter',
    github: 'fab fa-github',
    linkedin: 'fab fa-linkedin',
    website: 'fas fa-globe'
  };
  const cls = ICONS[kind] || 'fas fa-link';
  return (
    <a className="social-btn" href={href} title={title} target="_blank" rel="noreferrer">
      <i className={cls}></i>
    </a>);

}

function ProfileCard({ portrait, name, title, bioHtml, socials = [], past = false }) {
  return (
    <div className={`profile${past ? ' past' : ''}`}>
      <img className="profile-portrait" src={portrait} alt={name} />
      <div className="profile-name">{name}</div>
      <div className="profile-title">{title}</div>
      {!past && bioHtml ? <p className="profile-bio" dangerouslySetInnerHTML={{ __html: bioHtml }} /> : null}
      {!past && socials.length > 0 ?
      <div className="profile-socials">
          {socials.map((s, i) => <SocialIcon key={i} {...s} />)}
        </div> :
      null}
    </div>);

}

function PiCard({ portrait, name, title, bioHtml, socials = [] }) {
  return (
    <div className="pi-card">
      <img className="profile-portrait" src={portrait} alt={name} />
      <div className="pi-meta">
        <h2 className="pi-name">{name}</h2>
        <div className="pi-role">{title}</div>
        <p className="pi-bio" dangerouslySetInnerHTML={{ __html: bioHtml }} />
        <div className="profile-socials">
          {socials.map((s, i) => <SocialIcon key={i} {...s} />)}
        </div>
      </div>
    </div>);

}

// ----------------------------------------------------------------
// JOB CARD
// ----------------------------------------------------------------
function JobCard({ icon, name, subtitle, tabs }) {
  const [active, setActive] = useState(0);
  return (
    <div className="job-card">
      <div className="job-header">
        <img className="job-icon" src={icon} alt="" />
        <h5 className="job-name">{name}</h5>
        <div className="job-subtitle">{subtitle}</div>
      </div>
      <div className="nav-pills">
        {tabs.map((t, i) =>
        <div key={i} className={`nav-pill${active === i ? ' active' : ''}`}
        onClick={() => setActive(i)}>{t.label}</div>
        )}
      </div>
      <div className="job-content">
        {tabs[active].sections.map((s, i) =>
        <div key={i}>
            <h6>{s.heading}</h6>
            {s.body && <p dangerouslySetInnerHTML={{ __html: s.body }} />}
            {s.bullets &&
          <ul>{s.bullets.map((b, j) => <li key={j}>{b}</li>)}</ul>
          }
          </div>
        )}
      </div>
    </div>);

}

// ----------------------------------------------------------------
// SOFTWARE CARD
// ----------------------------------------------------------------
function SoftwareCard({ logo, name, tagline, children }) {
  return (
    <div className="software-card">
      <div className="software-logo-col">
        <img src={logo} className="software-logo" alt={name} />
      </div>
      <div className="software-body">
        <h3 className="software-name">{name}</h3>
        <h6 className="software-tagline">{tagline}</h6>
        <div className="software-text">{children}</div>
      </div>
    </div>);

}

// ----------------------------------------------------------------
// PUBLICATIONS
// ----------------------------------------------------------------
function PublicationCard({ pub }) {
  const [copied, setCopied] = useState(null);
  const copy = (text, which) => {
    const done = () => {
      setCopied(which);
      setTimeout(() => setCopied(null), 1400);
    };
    // Try modern API first
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(done).catch(() => fallback(text, done));
    } else {
      fallback(text, done);
    }
  };
  const fallback = (text, done) => {
    try {
      const ta = document.createElement('textarea');
      ta.value = text;
      ta.setAttribute('readonly', '');
      ta.style.cssText = 'position:fixed;top:0;left:0;opacity:0;pointer-events:none;';
      document.body.appendChild(ta);
      ta.select();
      ta.setSelectionRange(0, text.length);
      const ok = document.execCommand('copy');
      document.body.removeChild(ta);
      if (ok) done();
    } catch (e) {
      console.warn('Copy failed', e);
    }
  };
  return (
    <article className="pub-card">
      <div className="pub-year">{pub.year}</div>
      <div className="pub-main">
        <h4 className="pub-title">{pub.title}</h4>
        <p className="pub-text">
          {pub.authors} · <span className="pub-venue">{pub.venue}</span>
        </p>
      </div>
      <div className="pub-actions">
        <a className="pub-btn" href={pub.pdf} target="_blank" rel="noreferrer">
          <i className="fas fa-file-pdf"></i> PDF
        </a>
        <a className="pub-btn" href={pub.doi} target="_blank" rel="noreferrer">
          <i className="fas fa-link"></i> DOI
        </a>
        <button className={`pub-btn${copied === 'apa' ? ' copied' : ''}`} onClick={() => copy(pub.apa, 'apa')}>
          <i className="fas fa-quote-right"></i> {copied === 'apa' ? 'Copied' : 'Cite'}
        </button>
      </div>
    </article>);

}

function Publications() {
  const [pubs, setPubs] = useState(null);
  const [filter, setFilter] = useState('all');

  useEffect(() => {
    fetch('data/publications.json').
    then((r) => r.json()).
    then(setPubs).
    catch(() => setPubs([]));
  }, []);

  if (!pubs) return <div style={{ textAlign: 'center', padding: 60, color: '#7b7b7b' }}>Loading publications…</div>;

  const years = Array.from(new Set(pubs.map((p) => p.year))).sort((a, b) => b - a);
  const filtered = filter === 'all' ? pubs : pubs.filter((p) => p.year === filter);

  return (
    <div className="pubs">
      <div className="pub-toolbar">
        <h6 className="eyebrow">Filter by year</h6>
        <div className="pub-filters">
          <button className={`filter-pill${filter === 'all' ? ' active' : ''}`}
          onClick={() => setFilter('all')}>All ({pubs.length})</button>
          {years.map((y) =>
          <button key={y}
          className={`filter-pill${filter === y ? ' active' : ''}`}
          onClick={() => setFilter(y)}>{y}</button>
          )}
        </div>
      </div>

      <div className="pub-grid">
        {filtered.map((p) => <PublicationCard key={p.id} pub={p} />)}
      </div>
      {filtered.length === 0 &&
      <div style={{ textAlign: 'center', padding: 40, color: '#7b7b7b' }}>No publications for {filter}.</div>
      }
    </div>);

}

// ----------------------------------------------------------------
// FOOTER
// ----------------------------------------------------------------
function Footer({ onNavigate }) {
  const go = (id) => (e) => {e.preventDefault();onNavigate && onNavigate(id);};
  return (
    <footer className="site-footer">
      <div className="container">
        <div className="footer-col footer-brand">
          <span className="logo">samuk lab</span>
          <p>An evolutionary genomics lab in the EEOB Department at UC Riverside, studying how populations adapt and form new species.</p>
        </div>
        <div className="footer-col">
          <h6>Lab</h6>
          <ul>
            <li><a href="#research" onClick={go('research')}>Research</a></li>
            <li><a href="#people" onClick={go('people')}>People</a></li>
            <li><a href="#publications" onClick={go('publications')}>Publications</a></li>
            <li><a href="#software" onClick={go('software')}>Software</a></li>
          </ul>
        </div>
        <div className="footer-col">
          <h6>Connect</h6>
          <ul>
            <li><a href="#join" onClick={go('join')}>Join the lab</a></li>
            <li><a href="#contact" onClick={go('contact')}>Contact</a></li>
            <li><a href="https://github.com/ksamuk" target="_blank" rel="noreferrer">GitHub</a></li>
            <li><a href="https://scholar.google.com/citations?user=6SUFQxQAAAAJ&hl=en" target="_blank" rel="noreferrer">Google Scholar</a></li>
          </ul>
        </div>
        <div className="footer-col">
          <h6>Location</h6>
          <ul>
            <li><a>EEOB Biology Dept</a></li>
            <li><a>UC Riverside</a></li>
            <li><a>Spieth Hall, RM 1229</a></li>
            <li><a>Riverside, CA 92521</a></li>
          </ul>
        </div>
      </div>
      <div className="container footer-bottom">
        <span>© {new Date().getFullYear()} The Samuk Lab. All rights reserved.</span>
        <span>Made at UC Riverside</span>
      </div>
    </footer>);

}

// Export to window
Object.assign(window, {
  Header, Hero, FeatureTrio, NewsCarousel,
  ResearchCard, ProfileCard, PiCard, JobCard,
  SoftwareCard, Publications, Footer, SocialIcon
});