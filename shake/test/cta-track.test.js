// Behaviour test for ctaTrackScript as it ships: the inline tracking
// script is taken from a built webwinkelverhuis.nl page and run against a
// minimal fake DOM (no dependencies, runs on the node that elm-test
// already needs). Usage: node cta-track.test.js <built-page.html>
//
// Guards the two regressions found on 30 Sep 2026: contact links that Elm
// renders after load (the scanner's "platform niet herkend" mailto) must
// still be counted, and a form that Elm re-creates must not count as a
// second formulier_gestart.
'use strict';
const fs = require('fs');

const html = fs.readFileSync(process.argv[2], 'utf8');
const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].map(m => m[1]);
const script = scripts.find(s => s.includes('formulier_gestart'));
if (!script) {
  console.error('cta-track: no tracking script with formulier_gestart in ' + process.argv[2]);
  process.exit(1);
}

// Supports exactly the selector shapes the script uses:
// a[href="..."] and a[href^="..."].
function matchesSelector(href, selector) {
  const exact = selector.match(/^a\[href="(.*)"\]$/);
  if (exact) return href === exact[1];
  const prefix = selector.match(/^a\[href\^="(.*)"\]$/);
  if (prefix) return href.startsWith(prefix[1]);
  throw new Error('cta-track test: unsupported selector ' + selector);
}

function link(href, text) {
  const ownListeners = [];
  const element = {
    textContent: text,
    ownListeners,
    getAttribute: name => (name === 'href' ? href : null),
    matches: selector => matchesSelector(href, selector),
    classList: { contains: () => false },
    addEventListener: (type, handler) => {
      if (type === 'click') ownListeners.push(handler);
    },
  };
  element.closest = selector => (selector === 'a' ? element : null);
  return element;
}

function formField(form) {
  return { closest: selector => (selector === 'form' ? form : null) };
}

// staticLinks are the links present at DOMContentLoaded, which
// querySelectorAll returns; later links only exist when clicked.
function freshPage(staticLinks = []) {
  const documentListeners = {};
  const events = [];
  const document = {
    addEventListener: (type, handler) => {
      (documentListeners[type] = documentListeners[type] || []).push(handler);
    },
    querySelectorAll: selector => staticLinks.filter(l => l.matches(selector)),
  };
  const gtag = (kind, name, params) => {
    if (kind === 'event') events.push({ name, params });
  };
  const window = { gtag };
  new Function('document', 'window', 'location', 'gtag', script)(
    document, window, { pathname: '/scan.html' }, gtag);
  const fire = (type, target) =>
    (documentListeners[type] || []).forEach(handler => handler({ target }));
  fire('DOMContentLoaded', null);
  return { fire, events };
}

// A click runs the link's own listeners, then bubbles to the document.
function clickElement(page, element) {
  element.ownListeners.forEach(handler => handler({ target: element }));
  page.fire('click', element);
}

function clickOn(page, href, text) {
  clickElement(page, link(href, text));
}

const failures = [];
function expect(description, condition) {
  if (!condition) failures.push(description);
}

{
  const page = freshPage();
  clickOn(page, 'mailto:jappie@webwinkelverhuis.nl?subject=Platform', 'Mail ons');
  const mails = page.events.filter(e => e.name === 'mail_klik');
  expect('a mailto link that appears after load gives one mail_klik', mails.length === 1);
  expect('mail_klik carries the link_url',
    mails.length === 1 && mails[0].params.link_url.startsWith('mailto:jappie@'));
}

for (const [href, eventName] of [
  ['mailto:jappie@webwinkelverhuis.nl', 'mail_klik'],
  ['https://wa.me/31644237437', 'whatsapp_klik'],
  ['tel:+31644237437', 'bel_klik'],
]) {
  const staticLink = link(href, 'footer');
  const page = freshPage([staticLink]);
  clickElement(page, staticLink);
  expect('a ' + eventName + ' link present at load counts exactly once, not once per binding',
    page.events.filter(e => e.name === eventName).length === 1);
}

{
  const page = freshPage();
  clickOn(page, 'https://wa.me/31644237437?text=Hallo', '');
  expect('a WhatsApp link gives whatsapp_klik',
    page.events.some(e => e.name === 'whatsapp_klik'));
  clickOn(page, 'https://example.com/', 'elders');
  expect('an unrelated link gives no contact event',
    page.events.filter(e => /_klik$/.test(e.name)).length === 1);
}

{
  const page = freshPage();
  page.fire('focusin', formField({ id: 'eerste' }));
  page.fire('focusin', formField({ id: 'eerste' }));
  page.fire('focusin', formField({ id: 'opnieuw-aangemaakt' }));
  const starts = page.events.filter(e => e.name === 'formulier_gestart');
  expect('a re-created form still counts as one formulier_gestart', starts.length === 1);
  expect('formulier_gestart carries the page',
    starts.length === 1 && starts[0].params.pagina === '/scan.html');
}

if (failures.length > 0) {
  console.error('cta-track tests failed:\n  ' + failures.join('\n  '));
  process.exit(1);
}
console.log('cta-track tests passed');
