/* eslint-disable */
/* Samuk Lab — Page assemblies */

const { useState: usePagesState, useEffect: usePagesEffect } = React;

const ASSETS = 'assets';
const HERO_OFFSET_HOME = 600;
const HERO_OFFSET_INNER = 400;

// ----------------------------------------------------------------
// HOME
// ----------------------------------------------------------------
function HomePage({ onNavigate, newsAutoMs = 7000, retypeEnabled = true }) {
  return (
    <>
      <Hero
        image={`${ASSETS}/images/hero_bg.webp`}
        kicker={<span className="kicker-text"><span>The Samuk Lab</span><span>UC Riverside</span></span>}
        title={<>Understanding evolution and the diversity of life through</>}
        retypeWords={retypeEnabled ? ['bioinformatics.', 'field studies.', 'simulations.', 'crispr/cas9.', 'genomics.'] : null}
      />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_HOME }}>
        <section className="feature-band">
          <div className="container">
            <h6 className="eyebrow">About the lab</h6>
            <h2 className="section-title">A collaborative team studying the genetic basis of how organisms adapt, change, and form new species.</h2>
            <FeatureTrio
              onAction={id => onNavigate(id)}
              items={[
                { icon: `${ASSETS}/icons/lightbulb_icon.png`, title: 'Who we are',
                  desc: 'We are a collaborative academic research lab located at the University of California, Riverside, led by <b>Dr. Kieran Samuk</b>.',
                  cta: { label: 'Meet the lab', target: 'people' } },
                { icon: `${ASSETS}/icons/helix_icon.png`, title: 'What we study',
                  desc: 'We use modern genomics to understand <b>evolution</b> — how populations of organisms change, adapt, and form new species.',
                  cta: { label: 'Learn more', target: 'research' } },
                { icon: `${ASSETS}/icons/cv_icon.png`, title: 'Join us',
                  desc: 'We are <b>recruiting</b> new graduate students. If our research sounds interesting, get in touch!',
                  cta: { label: 'See opportunities', target: 'join' } },
              ]}
            />
          </div>
        </section>

        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// ----------------------------------------------------------------
// RESEARCH
// ----------------------------------------------------------------
function ResearchPage({ onNavigate }) {
  return (
    <>
      <Hero image={`${ASSETS}/images/hero_research.webp`} title="Research" innerSub="The genetic basis of evolutionary change" inner />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_INNER }}>
        <section className="section">
          <div className="container">
            <h6 className="eyebrow">What we work on</h6>
            <div className="research-intro">
              <div className="research-intro-text">
                Our research is focused on understanding the <b>genetic basis of evolutionary change</b>. This includes understanding the genetic changes that allow organisms to <b>adapt to new environments</b>, as well as those involved in the <b>formation of new species</b>.
                <p style={{ marginTop: 16 }}>Recent work in the lab has specifically focused on the complex role that <b>recombination</b> plays in the processes of adaptation and speciation. We work with two model organisms: the threespine stickleback (a freshwater fish), and flies in the genus <i>Drosophila</i>.</p>
              </div>
              <figure>
                <img src={`${ASSETS}/images/study_orgs_simple.png`} alt="" />
                <figcaption>The threespine stickleback and <i>Drosophila melanogaster</i> — our study organisms (not to scale)</figcaption>
              </figure>
            </div>

            <div className="spacer-lg" />
            <h6 className="eyebrow">Current projects</h6>
            <div className="spacer-sm" />

            <div className="research-cards">
              <ResearchCard
                image={`${ASSETS}/images/research_1.png`}
                caption="Samuk et al. 2020 (Curr Biol)"
                title="The evolution of recombination rate"
                intro="Recombination rate is often treated as a static species-level trait. However, recent work has shown that natural populations vary considerably in recombination rate, and this variation has a genetic basis."
                bullets={[
                  'Studying the genetic basis of recombination rate variation in natural populations',
                  'Exploring the role natural selection plays in driving differences in recombination rate',
                  'Experimental studies of the evolution of recombination modifiers',
                ]}
              />

              <ResearchCard
                imageRight
                image={`${ASSETS}/images/research_2.png`}
                caption="Two chromosomes that differ by a single large chromosomal inversion."
                title="Chromosomal inversions"
                intro="Chromosomal inversions are a class of structural variant in which a segment of chromosome is flipped/reversed in orientation. Studies over the last 100 years have found that inversions often underlie local adaptation and reproductive isolation."
                bullets={[
                  'Creating synthetic chromosomal inversions using modern transgenic approaches',
                  'Testing hypotheses about inversion evolution using experimental evolution',
                  'Field studies of inversions in natural populations of D. pseudoobscura',
                ]}
              />

              <ResearchCard
                image={`${ASSETS}/images/research_3.png`}
                caption="New approaches are needed to accelerate modern breeding."
                title="Accelerating breeding with recombination modifiers"
                intro="Adaptation to new environments can be limited by the availability of optimal haplotypes — haplotypes that contain all the 'Good' alleles. A key parameter shaping haplotypic diversity is recombination rate."
                bullets={[
                  'How recombination rate could be optimized to accelerate artificial selection regimes',
                  'New methods for modifying recombination rate in breeding populations',
                  'Experimental studies of optimized recombination in laboratory populations',
                ]}
              />
            </div>
          </div>
        </section>
        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// ----------------------------------------------------------------
// PEOPLE
// ----------------------------------------------------------------
function PeoplePage({ onNavigate }) {
  const [data, setData] = usePagesState(null);
  const [loadError, setLoadError] = usePagesState(false);
  usePagesEffect(() => {
    fetch('data/people.json')
      .then(r => {
        if (!r.ok) throw new Error('HTTP ' + r.status);
        return r.json();
      })
      .then(d => setData({
        current: Array.isArray(d?.current) ? d.current : [],
        past:    Array.isArray(d?.past)    ? d.past    : [],
      }))
      .catch(() => { setLoadError(true); setData({ current: [], past: [] }); });
  }, []);

  const resolvePortrait = (p) => {
    if (!p) return `${ASSETS}/images/profile_empty.png`;
    return p.startsWith('../')
      ? `${ASSETS}/images/${p.slice(3)}`
      : `${ASSETS}/images/people/${p}`;
  };

  const pi = data?.current?.[0];

  return (
    <>
      <Hero image={`${ASSETS}/images/hero_people.webp`} title="People" innerSub="The lab past, present & future" inner />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_INNER }}>
        <section className="section">
          <div className="container">
            {!data ? (
              <div style={{ textAlign: 'center', padding: 60, color: '#7b7b7b' }}>Loading lab members…</div>
            ) : loadError ? (
              <div style={{ textAlign: 'center', padding: 60, color: '#7b7b7b' }}>
                Could not load lab members. Please try again later.
              </div>
            ) : (
              <>
                {pi && (
                  <PiCard
                    portrait={resolvePortrait(pi.portrait)}
                    name={pi.name}
                    title={pi.title}
                    bioHtml={pi.bio_html}
                    socials={pi.socials || []}
                  />
                )}

                {data.current.length > 1 && (
                  <>
                    <h6 className="eyebrow">Current Lab Members</h6>
                    <div className="profile-grid">
                      {data.current.slice(1).map(m => (
                        <ProfileCard
                          key={m.id}
                          portrait={resolvePortrait(m.portrait)}
                          name={m.name}
                          title={m.title}
                          bioHtml={m.bio_html}
                          socials={m.socials || []}
                        />
                      ))}
                    </div>
                  </>
                )}

                {data.past.length > 0 && (
                  <>
                    <div className="spacer-lg" />
                    <h6 className="eyebrow">Past Lab Members</h6>
                    <div className="profile-grid past">
                      {data.past.map(m => (
                        <ProfileCard
                          key={m.id} past
                          portrait={resolvePortrait(m.portrait)}
                          name={m.name}
                          title={m.title}
                        />
                      ))}
                    </div>
                  </>
                )}
              </>
            )}
          </div>
        </section>
        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// ----------------------------------------------------------------
// PUBLICATIONS
// ----------------------------------------------------------------
function PublicationsPage({ onNavigate }) {
  return (
    <>
      <Hero image={`${ASSETS}/images/hero_publications.webp`} title="Publications" innerSub="Research articles from the lab" inner />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_INNER }}>
        <section className="section">
          <div className="container">
            <h6 className="eyebrow">Selected work</h6>
            <h2 className="section-title">Peer-reviewed papers from the Samuk Lab</h2>
            <div className="spacer-md" />
            <Publications />
          </div>
        </section>
        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// ----------------------------------------------------------------
// JOIN
// ----------------------------------------------------------------
function JoinPage({ onNavigate }) {
  return (
    <>
      <Hero image={`${ASSETS}/images/hero_join.webp`} title="Join" innerSub="Come work with us" inner />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_INNER }}>
        <section className="section">
          <div className="container container-narrow">
            <h6 className="eyebrow">Come work with us</h6>
            <p className="join-lead">
              We are currently recruiting PhD students for the 2026–2027 admission cycle. If our work sounds interesting — please reach out to Dr. Samuk!
            </p>
          </div>
        </section>

        <section className="feature-band">
          <div className="container">
            <h6 className="eyebrow">Current opportunities</h6>
            <h2 className="section-title">Open positions in the lab</h2>
            <div className="spacer-md" />
            <div className="job-grid">
              <JobCard
                icon={`${ASSETS}/icons/grad_icon.png`}
                name="Graduate Students"
                subtitle="Train to become a scientist"
                tabs={[
                  { label: 'Info', sections: [
                    { heading: 'Description',
                      body: 'We are always seeking to recruit enthusiastic PhD students to learn and study along with us in the lab. All graduate positions in the lab are <b>fully funded</b>.' },
                    { heading: 'Start date',
                      body: 'Currently recruiting for Sept 2027 start dates (application deadline ~November 2026).' },
                  ]},
                  { label: 'Details', sections: [
                    { heading: 'Projects',
                      body: 'In the Samuk lab, projects generally evolve around these themes:',
                      bullets: ['Adaptation and speciation', 'The evolution of recombination rate', 'The evolution of chromosomal inversions', 'Population genetics/genomics', 'The genetics of speciation'] },
                  ]},
                  { label: 'Apply', sections: [
                    { heading: 'How to apply',
                      body: 'For full details about the position and instructions on how to apply, see the Samuk Lab PhD positions document. You can also email Dr. Samuk directly to discuss fit before applying.' },
                  ]},
                ]}
              />
              <JobCard
                icon={`${ASSETS}/icons/postdoc_icon.png`}
                name="Postdocs"
                subtitle="Lead an independent project"
                tabs={[
                  { label: 'Info', sections: [
                    { heading: 'Description',
                      body: 'We are open to postdoc applicants with their own funding or who want to work with us on grant proposals. Get in touch with Dr. Samuk to discuss possibilities.' },
                  ]},
                  { label: 'Details', sections: [
                    { heading: 'What we look for',
                      bullets: ['Strong publication record', 'Interest in evolutionary genomics', 'Independent project ideas welcome'] },
                  ]},
                  { label: 'Apply', sections: [
                    { heading: 'Contact',
                      body: 'Email Dr. Samuk with CV, cover letter, and 2–3 references.' },
                  ]},
                ]}
              />
              <JobCard
                icon={`${ASSETS}/icons/undergrad_icon.png`}
                name="Undergraduates"
                subtitle="Get research experience"
                tabs={[
                  { label: 'Info', sections: [
                    { heading: 'Description',
                      body: 'We frequently have positions for UCR undergraduates interested in genomics, evolution, and computational biology. Both wet-lab and bioinformatics projects are available.' },
                  ]},
                  { label: 'Details', sections: [
                    { heading: 'What you\'ll do',
                      bullets: ['Hands-on work with stickleback or Drosophila', 'Population genomics analyses', 'Co-author on papers and conference presentations'] },
                  ]},
                  { label: 'Apply', sections: [
                    { heading: 'Contact', body: 'Send an email expressing interest with a short description of your background and goals.' },
                  ]},
                ]}
              />
              <JobCard
                icon={`${ASSETS}/icons/manager_icon.png`}
                name="Lab Manager"
                subtitle="Help us run the lab"
                tabs={[
                  { label: 'Info', sections: [
                    { heading: 'Description',
                      body: 'We are not currently hiring a lab manager, but we are always happy to hear from prospective candidates with relevant experience.' },
                  ]},
                  { label: 'Details', sections: [
                    { heading: 'Useful background',
                      bullets: ['Wet-lab molecular biology', 'Fly or fish husbandry', 'Lab safety + ordering experience'] },
                  ]},
                  { label: 'Apply', sections: [
                    { heading: 'Contact', body: 'Email Dr. Samuk with a CV — we\'ll keep your application on file.' },
                  ]},
                ]}
              />
            </div>
          </div>
        </section>

        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// ----------------------------------------------------------------
// SOFTWARE
// ----------------------------------------------------------------
function SoftwarePage({ onNavigate }) {
  return (
    <>
      <Hero image={`${ASSETS}/images/hero_software.webp`} title="Software" innerSub="Open-source tools from the lab" inner />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_INNER }}>
        <section className="section">
          <div className="container">
            <h6 className="eyebrow">Software we've created</h6>
            <p className="software-intro">
              As part of our research, we create software tools to help distill information from genomic data. We always make these tools freely available to other investigators, and hope that they make analyses faster, more accurate, and more reproducible.
            </p>
            <div className="spacer-md" />

            <SoftwareCard
              logo={`${ASSETS}/images/pixy_logo.png`}
              name="pixy"
              tagline="Unbiased estimation of nucleotide diversity with missing data"
            >
              <p>
                Co-developed with <a href="https://katharinekorunes.weebly.com/" target="_blank" rel="noreferrer">Dr. Katharine Korunes</a>, <b>pixy</b> is a command-line tool for painlessly and correctly estimating average nucleotide diversity within (π) and between (d<sub>xy</sub>) populations from a VCF. In particular, pixy facilitates these calculations using VCFs containing invariant sites, missing sites, and missing genotypes.
              </p>
              <ul className="software-links">
                <li><a href="https://doi.org/10.1111/1755-0998.13326" target="_blank" rel="noreferrer"><i className="fas fa-file-pdf"></i> Paper</a></li>
                <li><a href="https://github.com/ksamuk/pixy" target="_blank" rel="noreferrer"><i className="fab fa-github"></i> GitHub</a></li>
                <li><a href="https://pixy.readthedocs.io/en/latest/" target="_blank" rel="noreferrer"><i className="fas fa-book"></i> Docs</a></li>
              </ul>
            </SoftwareCard>

            <SoftwareCard
              logo={`${ASSETS}/images/vcfsim_logo.png`}
              name="vcfsim"
              tagline="Realistic VCFs from a few command-line arguments"
            >
              <p>
                Co-developed with <b>Paimon Goulart</b>, <b>vcfsim</b> is a command-line tool for generating simulated VCFs. It combines a coalescent simulation backend (<a href="https://tskit.dev/msprime/" target="_blank" rel="noreferrer">msprime</a>) with clean and efficient postprocessing to produce a wide variety of biologically realistic VCFs, with parameterized levels of missing data. vcfsim allows the creation of "all-sites" VCFs that contain both variant and invariant sites — making it a natural companion to pixy.
              </p>
              <ul className="software-links">
                <li><a href="https://github.com/samuk-lab/vcfsim" target="_blank" rel="noreferrer"><i className="fab fa-github"></i> GitHub</a></li>
                <li><a href="https://bioconda.github.io/recipes/vcfsim/README.html" target="_blank" rel="noreferrer"><i className="fas fa-box"></i> Bioconda</a></li>
              </ul>
            </SoftwareCard>

            <SoftwareCard
              logo={`${ASSETS}/images/syntr_logo.png`}
              name="syntR"
              tagline="Reproducible identification of chromosomal rearrangements"
            >
              <p>
                Co-developed with <a href="https://www.kateostevik.com/" target="_blank" rel="noreferrer">Dr. Kate Ostevik</a>, <b>syntR</b> is an R package for the reproducible identification of synteny blocks and chromosomal rearrangements via comparison of two genetic maps. syntR implements an error-aware clustering algorithm specifically designed for the highly linear structure of comparative genetic map data.
              </p>
              <ul className="software-links">
                <li><a href="https://academic.oup.com/genetics/article/214/4/1031/5930528?login=true" target="_blank" rel="noreferrer"><i className="fas fa-file-pdf"></i> Paper</a></li>
                <li><a href="https://ksamuk.github.io/syntR/" target="_blank" rel="noreferrer"><i className="fab fa-github"></i> GitHub</a></li>
                <li><a href="https://www.samuklab.com/syntR/articles/syntr_tutorial.html" target="_blank" rel="noreferrer"><i className="fas fa-book"></i> Tutorial</a></li>
              </ul>
            </SoftwareCard>
          </div>
        </section>
        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// ----------------------------------------------------------------
// CONTACT
// ----------------------------------------------------------------
function ContactPage({ onNavigate }) {
  return (
    <>
      <Hero image={`${ASSETS}/images/hero_contact.webp`} title="Contact" innerSub="Get in touch" inner />
      <main className="page-main" style={{ marginTop: HERO_OFFSET_INNER }}>
        <section className="section">
          <div className="container">
            <h6 className="eyebrow">How to find us</h6>
            <h2 className="section-title">We'd love to hear from prospective students, collaborators, and curious folks.</h2>
            <div className="spacer-md" />
            <div className="contact-grid">
              <div>
                <div className="contact-card">
                  <h4>Email</h4>
                  Dr. Samuk can be reached at <a href="mailto:ksamuk@ucr.edu">ksamuk@ucr.edu</a>.
                </div>
                <div className="contact-card">
                  <h4>Mailing Address</h4>
                  <address>
                    EEOB Biology Dept<br />
                    UC Riverside<br />
                    RM1229 Spieth – Samuk Lab<br />
                    3401 Watkins Dr<br />
                    Riverside, CA 92521
                  </address>
                </div>
                <div className="contact-card">
                  <h4>Around the web</h4>
                  <div className="profile-socials" style={{ justifyContent: 'flex-start', marginTop: 8 }}>
                    <SocialIcon kind="github" href="https://github.com/ksamuk" title="GitHub" />
                    <SocialIcon kind="scholar" href="https://scholar.google.com/citations?user=6SUFQxQAAAAJ&hl=en" title="Google Scholar" />
                    <SocialIcon kind="twitter" href="https://twitter.com/ksamuk" title="Twitter" />
                    <SocialIcon kind="cv" href="https://www.samuklab.com/files/cv/samuk_cv.pdf" title="CV" />
                  </div>
                </div>
              </div>
              <div className="contact-card contact-card-map">
                <iframe
                  title="Samuk Lab — Spieth Hall, UC Riverside"
                  src="https://www.openstreetmap.org/export/embed.html?bbox=-117.3316%2C33.9679%2C-117.3216%2C33.9779&layer=mapnik&marker=33.97285795155602%2C-117.32659649680336"
                  loading="lazy"
                  referrerPolicy="no-referrer-when-downgrade"
                />
                <a className="map-link" href="https://www.google.com/maps/place/Spieth+Hall/@33.97285795,-117.32659649,17z" target="_blank" rel="noreferrer">Open larger map →</a>
              </div>
            </div>
          </div>
        </section>
        <Footer onNavigate={onNavigate} />
      </main>
    </>
  );
}

// Export
Object.assign(window, {
  HomePage, ResearchPage, PeoplePage, PublicationsPage,
  JoinPage, SoftwarePage, ContactPage,
});
