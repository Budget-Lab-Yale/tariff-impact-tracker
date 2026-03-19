# Tariff Impacts Report — HTML Transformation Specification

## Purpose
The Budget Lab's tariff impact tracker Python script generates a source HTML file:
`https://raw.githubusercontent.com/Budget-Lab-Yale/tariff-impact-tracker/refs/heads/main/website/html/tariff_impacts_report.html`

This spec documents the exact transformations applied to that source to produce the Drupal custom HTML block version, saved at:
`C:\Users\ask76\OneDrive - Yale University\Documents\Budget Lab\tariff_impacts_report.html`

The Drupal version embeds live Datawrapper charts, uses the site's Drupal CSS class system for layout and styling, and omits elements handled elsewhere on the page (e.g. the date).

---

## Transformation Steps

Apply all steps in order. Do not modify article body text. Preserve all HTML entities (`&ndash;`, `&rsquo;`, `&mdash;`, `&amp;`, etc.) exactly as in the source.

---

### Step 1 — Prepend `<style>` block

Insert the following as the very first content of the file, before the `<article>` tag:

```html
<style>
@media (min-width: 64em) {
  .tariff-impacts-report section.layout,
  .tariff-impacts-report #key-takeaways {
    padding-inline: calc(100% / 6);
  }
}
.paragraph--key-takeaways .paragraph__title {
  text-align: center;
  font-family: Mallory, arial, sans-serif;
  font-size: 0.7501875469rem;
  font-weight: 800;
}
.paragraph--key-takeaways .paragraph__key-takeaways-list {
  list-style: none;
  margin: 35.53778px 0 0 0;
  padding: 0;
  counter-reset: key-takeaway-counter;
}
.paragraph--key-takeaways .paragraph__key-takeaways-list > * + * {
  border-top: 1px solid #63aaff;
}
.paragraph--key-takeaways .paragraph__key-takeaway-item {
  display: grid;
  grid-template-columns: auto 1fr;
  column-gap: 35.53778px;
  align-items: center;
  padding: 1em 0;
  margin: 0;
  counter-increment: key-takeaway-counter;
  line-height: 1.78;
}
.paragraph--key-takeaways .paragraph__key-takeaway-item:first-child {
  padding-top: 0;
}
.paragraph--key-takeaways .paragraph__key-takeaway-item:last-child {
  padding-bottom: 0;
}
.paragraph--key-takeaways .paragraph__key-takeaway-item::before {
  content: counter(key-takeaway-counter);
  font-family: Mallory, arial, sans-serif;
  font-size: clamp(1.776889rem, 1.579654321rem + 1.0957482167vw, 2.368593037rem);
  font-weight: 800;
  color: #101f5b;
  width: calc((calc(1440px - clamp(1.333rem, 0.3744243624rem + 5.3254202091vw, 4.2087269129rem) * 2) / 12) - 35.53778px);
}
.paragraph--key-takeaways .paragraph__key-takeaway-item p {
  margin: 0;
}
</style>
```

---

### Step 2 — Modify `<article>` tag

Change:
```html
<article class="tariff-impacts-report">
```
To:
```html
<article class="tariff-impacts-report ck-content">
```

---

### Step 3 — Remove report date line

Delete this line entirely (it appears immediately after the opening `<article>` tag):
```html
<p class="report-date">Updated: [date]</p>
```

---

### Step 4 — Transform Key Takeaways section

Replace the entire `<section id="key-takeaways">...</section>` block with the following structure. Preserve the text content of each `<li>` exactly; wrap it in `<div><p>...</p></div>`.

**Source structure:**
```html
<section id="key-takeaways">
<h2>Key Takeaways</h2>
<ul>
<li>[item text]</li>
...
</ul>
</section>
```

**Output structure:**
```html
<div id="key-takeaways" class="layout layout--margin-bottom-sm layout--one-column layout--color-scheme-light">
<section class="paragraph paragraph--key-takeaways" data-js="key-takeaways">
<h2 class="paragraph__title">Key Takeaways</h2>
<ol class="paragraph__key-takeaways-list">
<li class="paragraph__key-takeaway-item"><div><p>[item text]</p></div></li>
<li class="paragraph__key-takeaway-item"><div><p>[item text]</p></div></li>
...
</ol>
</section>
</div>
```

Notes:
- The outer wrapper is a `<div>` (not `<section>`), with `id="key-takeaways"`.
- The inner element is `<section class="paragraph paragraph--key-takeaways" data-js="key-takeaways">`.
- The `<h2>` gets class `paragraph__title`.
- The `<ul>` becomes `<ol class="paragraph__key-takeaways-list">`.
- Each `<li>` gets class `paragraph__key-takeaway-item` and its content is wrapped in `<div><p>...</p></div>`. The original `<li>` content is inline text/HTML (no existing `<p>` tags) — wrap it entirely.

---

### Step 5 — Add layout classes to all regular sections

For every `<section id="...">` that is NOT `#key-takeaways` (already handled) and NOT `#appendix` (handled in Step 7), add `class="layout layout--margin-bottom-sm"`.

Sections to update (IDs as found in source):
- `introduction`
- `tariff-rates-and-revenue`
- `prices-consumer-passthrough`
- `labor-market`
- `industrial-output`
- `us-dollar`
- `imports-exports`
- `data-sources`

Change pattern: `<section id="[id]">` → `<section id="[id]" class="layout layout--margin-bottom-sm">`

---

### Step 6 — Wrap subsections in `#prices-consumer-passthrough`

Within `<section id="prices-consumer-passthrough">`, the content must be wrapped in `layout` divs to create spacing around the intro paragraph and each h3 subsection.

**Rule:** Every discrete block of content (intro paragraph group, and each h3-led subsection) gets wrapped in `<div class="layout layout--margin-bottom-sm">...</div>`.

The source content of this section (after the `<h2>`) is structured as follows. Wrap each group as shown:

```html
<section id="prices-consumer-passthrough" class="layout layout--margin-bottom-sm">
<h2>Prices and Consumer Passthrough</h2>

<div class="layout layout--margin-bottom-sm">
<p>[intro paragraph — "One of the most debated questions..."]</p>
</div>

<div class="layout layout--margin-bottom-sm">
<h3>Overall Goods Prices</h3>
<p>[paragraph 1]</p>
<p>[paragraph 2 — "Versions of Figures 4 and 5..."]</p>
[F4 embed]
[F5 embed]
</div>

<div class="layout layout--margin-bottom-sm">
<h3>Imported PCE Goods Prices</h3>
<p>[paragraph]</p>
[F6 embed]
[F7 embed]
</div>

<div class="layout layout--margin-bottom-sm">
<h3>Implied Consumer Passthrough</h3>
<p>[paragraph 1]</p>
<p>[paragraph 2]</p>
<p>[paragraph 3]</p>
<p>[paragraph 4]</p>
<p>[paragraph 5]</p>
[T1a embed]
[T1b embed]
</div>

<div class="layout layout--margin-bottom-sm">
<h3>Import Prices</h3>
<p>[paragraph]</p>
[F8 embed]
</div>

</section>
```

The `<h2>` itself is NOT wrapped — it stays directly inside `<section>`.

---

### Step 7 — Transform appendix section

Change:
```html
<section id="appendix">
```
To:
```html
<section id="appendix" class="layout layout--margin-bottom-sm layout--one-column layout--color-scheme-light">
```

---

### Step 8 — Replace figure/table placeholders with Datawrapper embeds

Every `<div class="figure-placeholder" id="[id]" ...>...</div>` block must be replaced with a wrapped Datawrapper embed. The wrapper provides vertical spacing.

**Output structure for each figure/table:**
```html
<div style="margin-top:35.53778px;margin-bottom:35.53778px;">[DATAWRAPPER_EMBED]</div>
```

Where `[DATAWRAPPER_EMBED]` is the embed HTML from the table below.

**Important:** The inner Datawrapper `<div>` carries its own `style="min-height:XXXpx"` and `id="datawrapper-vis-XXXXX"`. Do NOT put the margin on that inner div — it must be on the outer wrapper div, because the Datawrapper JavaScript modifies the inner div's style on load and would overwrite any margin set there.

#### Complete embed table

| Figure ID | Datawrapper embed HTML |
|-----------|------------------------|
| F1 | `<div style="min-height:452px" id="datawrapper-vis-Sx7V4"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/Sx7V4/embed.js" charset="utf-8" data-target="#datawrapper-vis-Sx7V4"></script><noscript><img src="https://datawrapper.dwcdn.net/Sx7V4/full.png" alt="Figure 1. Daily Effective Tariff Rate (Import-Weighted) (Line chart)" /></noscript></div>` |
| F2 | `<div style="min-height:423px" id="datawrapper-vis-LOBeQ"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/LOBeQ/embed.js" charset="utf-8" data-target="#datawrapper-vis-LOBeQ"></script><noscript><img src="https://datawrapper.dwcdn.net/LOBeQ/full.png" alt="Figure 2. Effective Tariff Rate (Line chart)" /></noscript></div>` |
| F3 | `<div style="min-height:442px" id="datawrapper-vis-HUj1V"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/HUj1V/embed.js" charset="utf-8" data-target="#datawrapper-vis-HUj1V"></script><noscript><img src="https://datawrapper.dwcdn.net/HUj1V/full.png" alt="Figure 3. Customs Duty Revenue (Inflation-Adjusted) (Line chart)" /></noscript></div>` |
| F4 | `<div style="min-height:481px" id="datawrapper-vis-fMG7d"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/fMG7d/embed.js" charset="utf-8" data-target="#datawrapper-vis-fMG7d"></script><noscript><img src="https://datawrapper.dwcdn.net/fMG7d/full.png" alt="Figure 4. PCE Core Goods &amp; Durables Prices (Line chart)" /></noscript></div>` |
| F5 | `<div style="min-height:504px" id="datawrapper-vis-WwAfY"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/WwAfY/embed.js" charset="utf-8" data-target="#datawrapper-vis-WwAfY"></script><noscript><img src="https://datawrapper.dwcdn.net/WwAfY/full.png" alt="Figure 5. PCE Goods Prices: Deviation from Trend (Grouped column chart)" /></noscript></div>` |
| F6 | `<div style="min-height:468px" id="datawrapper-vis-gFGab"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/gFGab/embed.js" charset="utf-8" data-target="#datawrapper-vis-gFGab"></script><noscript><img src="https://datawrapper.dwcdn.net/gFGab/full.png" alt="Figure 6. Imported PCE Core Goods &amp; Durables Prices (Line chart)" /></noscript></div>` |
| F7 | `<div style="min-height:504px" id="datawrapper-vis-eI4YL"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/eI4YL/embed.js" charset="utf-8" data-target="#datawrapper-vis-eI4YL"></script><noscript><img src="https://datawrapper.dwcdn.net/eI4YL/full.png" alt="Figure 7. Imported PCE Goods Prices: Deviation from Trend (Grouped column chart)" /></noscript></div>` |
| T1a | `<div style="min-height:426px" id="datawrapper-vis-VRO87"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/VRO87/embed.js" charset="utf-8" data-target="#datawrapper-vis-VRO87"></script><noscript><img src="https://datawrapper.dwcdn.net/VRO87/full.png" alt="Table 1a. Implied Passthrough — Imported PCE Core and Durable Goods Prices (June 2025) (Table)" /></noscript></div>` |
| T1b | `<div style="min-height:430px" id="datawrapper-vis-e3xRP"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/e3xRP/embed.js" charset="utf-8" data-target="#datawrapper-vis-e3xRP"></script><noscript><img src="https://datawrapper.dwcdn.net/e3xRP/full.png" alt="Table 1b. Implied Passthrough — Imported PCE Core and Durable Goods Prices (December 2025) (Table)" /></noscript></div>` |
| F8 | `<div style="min-height:487px" id="datawrapper-vis-D1hao"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/D1hao/embed.js" charset="utf-8" data-target="#datawrapper-vis-D1hao"></script><noscript><img src="https://datawrapper.dwcdn.net/D1hao/full.png" alt="Figure 8. Non-Petroleum Import Prices (Line chart)" /></noscript></div>` |
| F9 | `<div style="min-height:453px" id="datawrapper-vis-itKu3"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/itKu3/embed.js" charset="utf-8" data-target="#datawrapper-vis-itKu3"></script><noscript><img src="https://datawrapper.dwcdn.net/itKu3/full.png" alt="Figure 9. Tariff-Exposed Employment Index (Line chart)" /></noscript></div>` |
| F10 | `<div style="min-height:453px" id="datawrapper-vis-wATUD"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/wATUD/embed.js" charset="utf-8" data-target="#datawrapper-vis-wATUD"></script><noscript><img src="https://datawrapper.dwcdn.net/wATUD/full.png" alt="Figure 10. Manufacturing Tariff-Exposed Employment Index (Line chart)" /></noscript></div>` |
| F11 | `<div style="min-height:440px" id="datawrapper-vis-ylolw"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/ylolw/embed.js" charset="utf-8" data-target="#datawrapper-vis-ylolw"></script><noscript><img src="https://datawrapper.dwcdn.net/ylolw/full.png" alt="Figure 11. Manufacturing Industrial Production (Line chart)" /></noscript></div>` |
| F12 | `<div style="min-height:452px" id="datawrapper-vis-OOsLi"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/OOsLi/embed.js" charset="utf-8" data-target="#datawrapper-vis-OOsLi"></script><noscript><img src="https://datawrapper.dwcdn.net/OOsLi/full.png" alt="Figure 12. Nominal Effective Exchange Rates (Daily) (Line chart)" /></noscript></div>` |
| F13 | `<div style="min-height:461px" id="datawrapper-vis-vMfoU"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/vMfoU/embed.js" charset="utf-8" data-target="#datawrapper-vis-vMfoU"></script><noscript><img src="https://datawrapper.dwcdn.net/vMfoU/full.png" alt="Figure 13. Real US Imports and Exports (Line chart)" /></noscript></div>` |
| F14 | `<div style="min-height:530px" id="datawrapper-vis-zjCNP"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/zjCNP/embed.js" charset="utf-8" data-target="#datawrapper-vis-zjCNP"></script><noscript><img src="https://datawrapper.dwcdn.net/zjCNP/full.png" alt="Figure 14. Trade Deviation from Trend (Grouped column chart)" /></noscript></div>` |
| F15 | `<div style="min-height:438px" id="datawrapper-vis-6fA3s"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/6fA3s/embed.js" charset="utf-8" data-target="#datawrapper-vis-6fA3s"></script><noscript><img src="https://datawrapper.dwcdn.net/6fA3s/full.png" alt="Figure 15. Cumulative Import Gap vs. Trend (Line chart)" /></noscript></div>` |
| TA1 | `<div style="min-height:370px" id="datawrapper-vis-my9Dq"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/my9Dq/embed.js" charset="utf-8" data-target="#datawrapper-vis-my9Dq"></script><noscript><img src="https://datawrapper.dwcdn.net/my9Dq/full.png" alt="Table A1. Implied Consumer Passthrough — All PCE Goods (June 2025) (Table)" /></noscript></div>` |
| TA2 | `<div style="min-height:392px" id="datawrapper-vis-XsAII"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/XsAII/embed.js" charset="utf-8" data-target="#datawrapper-vis-XsAII"></script><noscript><img src="https://datawrapper.dwcdn.net/XsAII/full.png" alt="Table A2. Implied Consumer Passthrough — All PCE Goods (December 2025) (Table)" /></noscript></div>` |
| FA1 | `<div style="min-height:518px" id="datawrapper-vis-0Gi6n"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/0Gi6n/embed.js" charset="utf-8" data-target="#datawrapper-vis-0Gi6n"></script><noscript><img src="https://datawrapper.dwcdn.net/0Gi6n/full.png" alt="Figure A1. Daily Effective Tariff Rate by Authority (Stacked area chart)" /></noscript></div>` |
| FA2 | `<div style="min-height:508px" id="datawrapper-vis-QLfvR"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/QLfvR/embed.js" charset="utf-8" data-target="#datawrapper-vis-QLfvR"></script><noscript><img src="https://datawrapper.dwcdn.net/QLfvR/full.png" alt="Figure A2. PCE Core Goods &amp; Durables Prices (Log-Linear Trend) (Line chart)" /></noscript></div>` |
| FA3 | `<div style="min-height:531px" id="datawrapper-vis-JBzX3"><script type="text/javascript" defer src="https://datawrapper.dwcdn.net/JBzX3/embed.js" charset="utf-8" data-target="#datawrapper-vis-JBzX3"></script><noscript><img src="https://datawrapper.dwcdn.net/JBzX3/full.png" alt="Figure A3. PCE Goods Prices: Deviation from Log-Linear Trend (Grouped column chart)" /></noscript></div>` |

---

## Verification checklist

After producing the output file, confirm:

- [ ] `<style>` block is the first element in the file
- [ ] `<article>` has both `tariff-impacts-report` and `ck-content` classes
- [ ] No `<p class="report-date">` present
- [ ] `#key-takeaways` is a `<div>`, not a `<section>`, with the four layout/color-scheme classes
- [ ] Key takeaways list is `<ol>` (not `<ul>`) and items use `paragraph__key-takeaway-item` class with `<div><p>` wrapping
- [ ] All 8 regular sections have `class="layout layout--margin-bottom-sm"`
- [ ] `#prices-consumer-passthrough` intro paragraph and all 4 h3 subsections are each wrapped in `<div class="layout layout--margin-bottom-sm">`
- [ ] `#appendix` has `layout--one-column layout--color-scheme-light` in addition to standard layout classes
- [ ] All 22 figure placeholders replaced; none remain (search for `figure-placeholder`)
- [ ] Each embed is wrapped in `<div style="margin-top:35.53778px;margin-bottom:35.53778px;">` outer div
