(() => {
  const shell = document.querySelector('.post-shell');
  const content = document.querySelector('.post-content');
  if (!shell || !content) return;

  const headings = [...content.querySelectorAll('h2[id], h3[id]')];
  if (!headings.length) return;

  const toc = document.createElement('aside');
  toc.className = 'toc-panel';
  toc.dataset.blogToc = '';

  const nav = document.createElement('nav');
  let currentParent = '';

  headings.forEach((heading) => {
    const level = Number(heading.tagName.slice(1));
    if (level === 2) currentParent = heading.id;

    const link = document.createElement('a');
    link.className = `toc-link toc-level-${level}`;
    link.dataset.tocLevel = String(level);
    link.dataset.tocParent = level === 2 ? heading.id : currentParent;
    link.href = `#${heading.id}`;
    link.textContent = heading.textContent.trim();
    nav.appendChild(link);
  });

  toc.appendChild(nav);
  shell.appendChild(toc);

  const links = [...toc.querySelectorAll('.toc-link')];
  const setActive = () => {
    const current = headings.reduce(
      (active, heading) => heading.getBoundingClientRect().top < 130 ? heading : active,
      headings[0]
    );
    if (!current) return;

    const activeLink = links.find((link) => decodeURIComponent(link.hash.slice(1)) === current.id);
    const parent = activeLink?.dataset.tocLevel === '3' ? activeLink.dataset.tocParent : current.id;

    links.forEach((link) => {
      const isActive = decodeURIComponent(link.hash.slice(1)) === current.id;
      link.classList.toggle('is-active', isActive);
      link.classList.toggle('is-visible', link.dataset.tocLevel === '2' || link.dataset.tocParent === parent);
    });
  };

  setActive();
  document.addEventListener('scroll', setActive, { passive: true });
})();
