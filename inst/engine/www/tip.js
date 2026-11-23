// hovercraft tip engine 0.42.0, built from its source; do not edit.
(() => {
  const VERSION = '0.42.0';
  if (window.Hovercraft) return;
  const noted = new Set();
  const given = (v) => v !== null && v !== undefined;
  const isObject = (v) => v !== null && typeof v === 'object' && !Array.isArray(v);
  const kindOf = (v) => (v === null ? 'null' : Array.isArray(v) ? 'a list' : typeof v);
  const quoted = (v) => JSON.stringify(v) ?? String(v);
  const oneOf = (values) => values.map((v) => JSON.stringify(v)).join(', ');
  const didYouMean = () => '.';
  const stamp = (detail) => ({ ...detail });
  const num = (value) => (Number.isFinite(value) ? Number(value.toPrecision(12)) : value);

  function decimalParts(abs) {
    if (abs === 0) return { digits: '0', dot: 1 };
    const m = /^(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/.exec(String(abs));
    const raw = `${m[1]}${m[2] ?? ''}`;
    let dot = m[1].length + (m[3] ? Number(m[3]) : 0);
    let i = 0;
    while (i < raw.length && raw[i] === '0') i++;
    if (i === raw.length) return { digits: '0', dot: 1 };
    dot -= i;
    return { digits: raw.slice(i), dot };
  }

  function roundDecimalEven(digits, dot, places) {
    if (digits === '0') return { digits: '0', dot: 1 };
    const keepLen = dot + places;
    if (keepLen < 0) return { digits: '0', dot: 1 };
    if (keepLen >= digits.length) return { digits, dot };
    const first = digits[keepLen];
    const restNonZero = /[^0]/.test(digits.slice(keepLen + 1));
    const lastKept = keepLen === 0 ? 0 : Number(digits[keepLen - 1]);
    const roundUp = first > '5' || (first === '5' && (restNonZero || lastKept % 2 === 1));
    if (!roundUp) return keepLen === 0 ? { digits: '0', dot: 1 } : { digits: digits.slice(0, keepLen), dot };
    if (keepLen === 0) return { digits: '1', dot: 1 - places };
    const out = digits.slice(0, keepLen).split('');
    for (let i = out.length - 1; i >= 0; i--) {
      if (out[i] !== '9') {
        out[i] = String(Number(out[i]) + 1);
        return { digits: out.join(''), dot };
      }
      out[i] = '0';
    }
    return { digits: `1${out.join('')}`, dot: dot + 1 };
  }

  function formatDecimalParts(digits, dot, fixedDigits) {
    if (fixedDigits !== null) {
      if (digits === '0') return fixedDigits > 0 ? `0.${'0'.repeat(fixedDigits)}` : '0';
      let intPart = '0';
      let fracPart = '';
      if (dot <= 0) {
        fracPart = `${'0'.repeat(-dot)}${digits}`;
      } else if (dot >= digits.length) {
        intPart = `${digits}${'0'.repeat(dot - digits.length)}`;
      } else {
        intPart = digits.slice(0, dot);
        fracPart = digits.slice(dot);
      }
      if (fixedDigits === 0) return intPart;
      return `${intPart}.${fracPart.padEnd(fixedDigits, '0')}`;
    }
    const trimmed = digits.replace(/0+$/, '');
    if (!trimmed) return '0';
    if (dot < -5 || dot > 21) {
      const exp = dot - 1;
      const mant = trimmed.length > 1 ? `${trimmed[0]}.${trimmed.slice(1)}` : trimmed;
      return `${mant}e${exp >= 0 ? `+${exp}` : exp}`;
    }
    if (dot <= 0) return `0.${'0'.repeat(-dot)}${trimmed}`;
    if (dot >= trimmed.length) return `${trimmed}${'0'.repeat(dot - trimmed.length)}`;
    return `${trimmed.slice(0, dot)}.${trimmed.slice(dot)}`;
  }

  const DOUBLE_BUF = new ArrayBuffer(8);
  const DOUBLE_VIEW = new DataView(DOUBLE_BUF);

  function roundDoubleExactBigInt(abs, places) {
    if (abs === 0) return { digits: '0', dot: 1 };
    DOUBLE_VIEW.setFloat64(0, abs, false);
    const hi = DOUBLE_VIEW.getUint32(0, false);
    const lo = DOUBLE_VIEW.getUint32(4, false);
    const expBits = (hi >>> 20) & 0x7ff;
    const mant = (BigInt(hi & 0xfffff) << 32n) | BigInt(lo);
    const m = expBits === 0 ? mant : mant | (1n << 52n);
    const e = expBits === 0 ? -1074 : expBits - 1075;
    const shift = e + places;
    const numScaled = m * 5n ** BigInt(places);
    let q;
    if (shift >= 0) {
      q = numScaled << BigInt(shift);
    } else {
      const den = 1n << BigInt(-shift);
      q = numScaled / den;
      const twiceR = (numScaled % den) << 1n;
      if (twiceR > den || (twiceR === den && (q & 1n) === 1n)) q += 1n;
    }
    if (q === 0n) return { digits: '0', dot: 1 };
    const s = q.toString();
    return { digits: s, dot: s.length - places };
  }

  const FORMAT_NUMBER_KEYS = ['digits', 'percent'];
  function format_number(value, options) {
    let digits = null;
    let percent = false;
    if (given(options)) {
      if (!isObject(options)) {
        const id = 'format_number|options';
        if (!noted.has(id)) {
          noted.add(id);
          console.warn(`Hovercraft: format_number options must be an object, got ${kindOf(options)}.`);
        }
      } else {
        for (const [key, val] of Object.entries(options)) {
          if (!FORMAT_NUMBER_KEYS.includes(key)) {
            const id = `format_number|${key}`;
            if (!noted.has(id)) {
              noted.add(id);
              console.warn(
                `Hovercraft: format_number does not take ${quoted(key)}; it takes ${oneOf(FORMAT_NUMBER_KEYS)}${didYouMean(key, FORMAT_NUMBER_KEYS)}`,
              );
            }
          } else if (key === 'digits' && given(val)) {
            if (Number.isInteger(val) && val >= 0 && val <= 100) digits = val;
            else {
              const id = `format_number|digits|${String(val)}`;
              if (!noted.has(id)) {
                noted.add(id);
                const got = quoted(val);
                console.warn(`Hovercraft: format_number "digits" must be an integer from 0 to 100, got ${got}.`);
              }
            }
          } else if (key === 'percent' && given(val)) {
            if (typeof val === 'boolean') percent = val;
            else {
              const id = `format_number|percent|${String(val)}`;
              if (!noted.has(id)) {
                noted.add(id);
                console.warn(`Hovercraft: format_number "percent" must be true or false, got ${quoted(val)}.`);
              }
            }
          }
        }
      }
    }
    if (typeof value !== 'number' || !Number.isFinite(value)) return '';
    const neg = value < 0;
    const abs = Math.abs(value);
    let rounded;
    if (digits === null) {
      rounded = decimalParts(Number(abs.toPrecision(15)));
    } else {
      const places = digits + (percent ? 2 : 0);
      if (abs < 1e21 && places <= 40) {
        const exact = decimalParts(abs.toFixed(100));
        rounded = roundDecimalEven(exact.digits, exact.dot, places);
      } else {
        rounded = roundDoubleExactBigInt(abs, places);
      }
    }
    const shiftedDot = rounded.digits === '0' ? 1 : rounded.dot + (percent ? 2 : 0);
    const body = formatDecimalParts(rounded.digits, shiftedDot, digits);
    const isZero = /^0(?:\.0+)?$/.test(body);
    return `${neg && !isZero ? '-' : ''}${body}${percent ? '%' : ''}`;
  }
  const MD_ESCAPABLE = '!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~';
  const MD_LINK_PREFIXES = ['http://', 'https://', 'mailto:'];
  const MD_LINK_BLANK = ['http://', 'https://'];
  const MD_LINK_REL = 'noopener noreferrer';
  const MD_BULLET = /^[-*+] /;
  const MD_ORDERED = /^([0-9]{1,9})\. /;
  const MD_PICKABLE =
    'button, [role="button"], [role="option"], [role="tab"], [role="menuitem"], [role="tooltip"], .hc-item';
  const mdSpace = (c) => c === ' ' || c === '\t';
  const mdTrim = (t) => t.replace(/^[ \t]+|[ \t]+$/g, '');
  const mdLower = (t) => t.replace(/[A-Z]/g, (c) => String.fromCharCode(c.charCodeAt(0) + 32));
  const mdStarts = (t, prefixes) => prefixes.some((p) => mdLower(t).startsWith(p));

  function mdBlocks(text) {
    const out = [];
    let para = null;
    let items = null;
    let blank = false;
    const endPara = () => {
      if (para) out.push(['p', para.join(' ')]);
      para = null;
    };
    const endList = () => {
      if (items) out.push([items[0], items[1], items[2].map((i) => i.join(' '))]);
      items = null;
    };
    for (const line of text.split(/\r\n|\r|\n/)) {
      if (!mdTrim(line)) {
        endPara();
        blank = true;
        continue;
      }
      const bullet = MD_BULLET.exec(line);
      const ordered = bullet ? null : MD_ORDERED.exec(line);
      if (bullet || ordered) {
        endPara();
        const kind = bullet ? 'ul' : 'ol';
        if (items && items[0] !== kind) endList();
        if (!items) items = [kind, ordered ? Number(ordered[1]) : 1, []];
        const words = mdTrim(line.slice((bullet ?? ordered)[0].length));
        items[2].push(words ? [words] : []);
      } else if (items && !blank) items[2][items[2].length - 1].push(mdTrim(line));
      else {
        endList();
        para = [...(para ?? []), mdTrim(line)];
      }
      blank = false;
    }
    endPara();
    endList();
    return out;
  }

  function mdEscaped(s) {
    const out = new Array(s.length).fill(false);
    for (let i = 0; i < s.length; ) {
      if (s[i] === '\\' && i + 1 < s.length && MD_ESCAPABLE.includes(s[i + 1])) {
        out[i + 1] = true;
        i += 2;
      } else i += 1;
    }
    return out;
  }

  function mdNext(s, escaped, char, inCode) {
    const next = new Array(s.length + 1).fill(-1);
    for (let i = s.length - 1; i >= 0; i -= 1) next[i] = s[i] === char && !escaped[i] && !inCode[i] ? i : next[i + 1];
    return next;
  }

  function mdRuns(s) {
    const runs = new Map();
    for (let i = 0; i < s.length; ) {
      if (s[i] !== '`') {
        i += 1;
        continue;
      }
      let j = i;
      while (j < s.length && s[j] === '`') j += 1;
      if (!runs.has(j - i)) runs.set(j - i, []);
      runs.get(j - i).push(i);
      i = j;
    }
    return runs;
  }

  function mdCodeMask(s, runs) {
    const inCode = new Array(s.length).fill(false);
    const pointer = new Map();
    for (let i = 0; i < s.length; ) {
      if (s[i] === '\\' && i + 1 < s.length && MD_ESCAPABLE.includes(s[i + 1])) i += 2;
      else if (s[i] === '`') {
        let j = i;
        while (j < s.length && s[j] === '`') j += 1;
        const starts = runs.get(j - i) ?? [];
        let p = pointer.get(j - i) ?? 0;
        while (p < starts.length && starts[p] < j) p += 1;
        pointer.set(j - i, p);
        if (p < starts.length) {
          const end = starts[p] + (j - i);
          inCode.fill(true, i, end);
          i = end;
        } else i = j;
      } else i += 1;
    }
    return inCode;
  }

  const mdCode = (raw) =>
    raw.length >= 2 && raw[0] === ' ' && raw.at(-1) === ' ' && /[^ ]/.test(raw) ? raw.slice(1, -1) : raw;

  function mdUnescape(raw) {
    let out = '';
    for (let i = 0; i < raw.length; ) {
      if (raw[i] === '\\' && i + 1 < raw.length && MD_ESCAPABLE.includes(raw[i + 1])) {
        out += raw[i + 1];
        i += 2;
      } else out += raw[i++];
    }
    return out;
  }

  function mdInline(s, links = true) {
    const tokens = [];
    let buf = '';
    const escaped = mdEscaped(s);
    const runs = mdRuns(s);
    const inCode = links ? mdCodeMask(s, runs) : null;
    const closeBracket = links ? mdNext(s, escaped, ']', inCode) : null;
    const closeParen = links ? mdNext(s, escaped, ')', inCode) : null;
    const pointer = new Map();
    const flush = () => {
      if (buf) tokens.push(['text', buf]);
      buf = '';
    };
    const linkAt = (i) => {
      const j = closeBracket[i + 1];
      if (j === -1 || j + 1 >= s.length || s[j + 1] !== '(') return null;
      const k = closeParen[j + 2];
      if (k === -1) return null;
      const raw = s.slice(j + 2, k);
      return raw && !/[ \t]/.test(raw) ? [j, k] : null;
    };
    const n = s.length;
    for (let i = 0; i < n; ) {
      const c = s[i];
      if (c === '\\' && i + 1 < n && MD_ESCAPABLE.includes(s[i + 1])) {
        buf += s[i + 1];
        i += 2;
      } else if (c === '`') {
        let j = i;
        while (j < n && s[j] === '`') j += 1;
        const starts = runs.get(j - i) ?? [];
        let p = pointer.get(j - i) ?? 0;
        while (p < starts.length && starts[p] < j) p += 1;
        pointer.set(j - i, p);
        if (p < starts.length) {
          flush();
          tokens.push(['code', mdCode(s.slice(j, starts[p]))]);
          i = starts[p] + (j - i);
        } else {
          buf += s.slice(i, j);
          i = j;
        }
      } else if (c === '[' && links && !(i > 0 && s[i - 1] === '!' && !escaped[i - 1]) && linkAt(i)) {
        const [j, k] = linkAt(i);
        const raw = s.slice(j + 2, k);
        flush();
        tokens.push(['link', mdUnescape(raw), mdInline(s.slice(i + 1, j), false), mdStarts(raw, MD_LINK_PREFIXES)]);
        i = k + 1;
      } else if (c === '*') {
        let j = i;
        while (j < n && s[j] === '*') j += 1;
        if (j - i > 3) buf += s.slice(i, j);
        else {
          flush();
          tokens.push({ count: j - i, canOpen: j < n && !mdSpace(s[j]), canClose: i > 0 && !mdSpace(s[i - 1]) });
        }
        i = j;
      } else {
        buf += c;
        i += 1;
      }
    }
    flush();
    return mdEmphasis(tokens);
  }

  function mdJoin(pieces) {
    const joined = [];
    for (const p of pieces) {
      if (p[0] === 'text' && joined.at(-1)?.[0] === 'text')
        joined[joined.length - 1] = ['text', joined.at(-1)[1] + p[1]];
      else if (p[0] !== 'text' || p[1]) joined.push(p);
    }
    return joined;
  }

  function mdEmphasis(tokens) {
    const stack = [];
    const tops = { em: [], strong: [] };
    const close = (kind) => {
      const pos = tops[kind].at(-1);
      for (const mark of stack.slice(pos + 1)) {
        mark[1] = 'text';
        tops[mark[0]].pop();
      }
      stack.length = pos + 1;
      stack.pop()[1] = 'closed';
      tops[kind].pop();
    };
    for (const tok of tokens) {
      if (Array.isArray(tok)) continue;
      const closed = [];
      if (tok.canClose) {
        const wants =
          tok.count === 3
            ? ['em', 'strong'].filter((k) => tops[k].length).sort((a, b) => tops[b].at(-1) - tops[a].at(-1))
            : [tok.count === 1 ? 'em' : 'strong'];
        for (const kind of wants) {
          if (tops[kind].length) {
            close(kind);
            closed.push(kind);
          }
        }
      }
      if (closed.length) {
        const used = closed.reduce((sum, k) => sum + (k === 'strong' ? 2 : 1), 0);
        tok.pieces = [
          ...closed.map((k) => ['close', k]),
          ...(tok.count > used ? [['text', '*'.repeat(tok.count - used)]] : []),
        ];
      } else if (tok.canOpen) {
        tok.opened = { 1: ['em'], 2: ['strong'], 3: ['em', 'strong'] }[tok.count].map((kind) => {
          const mark = [kind, 'open'];
          stack.push(mark);
          tops[kind].push(stack.length - 1);
          return mark;
        });
      } else tok.pieces = [['text', '*'.repeat(tok.count)]];
    }
    for (const mark of stack) mark[1] = 'text';
    const out = [];
    for (const tok of tokens) {
      if (Array.isArray(tok)) out.push(tok);
      else if (tok.opened)
        for (const [kind, state] of tok.opened)
          out.push(state === 'closed' ? ['open', kind] : ['text', kind === 'strong' ? '**' : '*']);
      else out.push(...tok.pieces);
    }
    return mdJoin(out);
  }

  const mdPlain = (pieces) =>
    pieces.map((p) => (p[0] === 'text' || p[0] === 'code' ? p[1] : p[0] === 'link' ? mdPlain(p[2]) : '')).join('');

  function mdLink(href, at) {
    const a = document.createElement('a');
    a.setAttribute('href', href);
    a.setAttribute('rel', MD_LINK_REL);
    if (mdStarts(href, MD_LINK_BLANK)) a.setAttribute('target', '_blank');
    a.addEventListener('click', (e) => {
      e.stopPropagation();
      const root = a.closest('[data-hovercraft-visual]');
      const detail = stamp(
        {
          href,
          by: 'reader',
          field: at.field,
          id: at.id,
          part: root?.dataset.hovercraftVisual ?? null,
          name: root?.dataset.hovercraftName ?? null,
        },
        a.ownerDocument,
      );
      (root ?? a).dispatchEvent(new CustomEvent('hovercraft:link', { bubbles: true, detail }));
    });
    return a;
  }

  function mdNodes(pieces, at) {
    const out = [];
    const open = [];
    const into = () => open.at(-1) ?? null;
    const add = (node) => (into() ? into().append(node) : out.push(node));
    for (const p of pieces) {
      if (p[0] === 'text') add(document.createTextNode(p[1]));
      else if (p[0] === 'code') add(Object.assign(document.createElement('code'), { textContent: p[1] }));
      else if (p[0] === 'open') {
        const el = document.createElement(p[1]);
        add(el);
        open.push(el);
      } else if (p[0] === 'close') open.pop();
      else if (p[3] && !at.words) {
        const a = mdLink(p[1], at);
        a.append(...mdNodes(p[2], at));
        add(a);
      } else for (const node of mdNodes(p[2], at)) add(node);
    }
    return out;
  }

  function markdown(el, text, { inline = false, field = null, id = null } = {}) {
    const at = {
      field: field ?? null,
      id: id === null || id === undefined ? null : String(id),
      words: Boolean(el.closest?.(MD_PICKABLE)),
    };
    el.classList.toggle('hc-md', !inline);
    el.classList.toggle('hc-md-inline', !!inline);
    if (text === null || text === undefined) {
      el.replaceChildren();
      return el;
    }
    const parts = mdBlocks(String(text));
    if (inline) {
      const pieces = [];
      for (const b of parts) {
        const marker = (i) => (b[0] === 'ul' ? '-' : `${b[1] + i}.`);
        const lines = b[0] === 'p' ? [b[1]] : b[2].map((item, i) => `${marker(i)} ${item}`.replace(/ +$/, ''));
        for (const line of lines) {
          if (pieces.length) pieces.push(['text', ' ']);
          for (const piece of mdInline(line)) pieces.push(piece);
        }
      }
      el.replaceChildren(...mdNodes(mdJoin(pieces), at));
      return el;
    }
    const nodes = parts.map((b) => {
      if (b[0] === 'p') {
        const p = document.createElement('p');
        p.append(...mdNodes(mdInline(b[1]), at));
        return p;
      }
      const list = document.createElement(b[0]);
      if (b[0] === 'ol' && b[1] !== 1) list.setAttribute('start', String(b[1]));
      for (const item of b[2]) {
        const li = document.createElement('li');
        li.append(...mdNodes(mdInline(item), at));
        list.append(li);
      }
      return list;
    });
    el.replaceChildren(...nodes);
    return el;
  }

  const MD_MARKS = ['\\', '`', '*', '[', ']'];
  markdown.escape = (text) => {
    if (text === null || text === undefined) return '';
    let out = String(text).replace(/\s*[\r\n]\s*/g, ' ');
    out = Array.from(out, (c) => (MD_MARKS.includes(c) ? `\\${c}` : c)).join('');
    const ordered = MD_ORDERED.exec(out);
    if (ordered) return `${ordered[1]}\\${out.slice(ordered[1].length)}`;
    return MD_BULLET.test(out) ? `\\${out}` : out;
  };

  markdown.plain = (text) => {
    if (text === null || text === undefined) return '';
    const words = [];
    for (const b of mdBlocks(String(text))) {
      if (b[0] === 'p') words.push(mdPlain(mdInline(b[1])));
      else for (const item of b[2]) words.push(mdPlain(mdInline(item)));
    }
    return words.join(' ');
  };
  Object.freeze(markdown);
  const TIP_NAMES = Object.freeze({
    attr: 'data-hovercraft-tip',
    css: 'hc-tip',
    who: 'Hovercraft',
    standalone: true,
  });

  function createTipEngine(env) {
    const NAMES = TIP_NAMES;
    const CSS = NAMES.css;
    const HINT_DELAY = 300;
    const PLACEMENTS = ['auto', 'top', 'bottom', 'left', 'right']; // tooltip_placement: bslib's words
    const PHONE = 600;
    const LEAVE_MS = 100;
    const GAP = 6;
    const CUT = 200;
    const LATER = ['rows', 'steps', 'spark', 'actions', 'html', 'visual'];
    const DASH = '–';
    const MODAL = 'dialog[open], [aria-modal="true"]';
    const MARKS = [
      'data-hovercraft-theme',
      'data-hovercraft-look',
      'data-hovercraft-palette',
      'data-hovercraft-decor',
      'data-hovercraft-motion-speed',
      'data-hovercraft-motion-type',
    ];
    const tipOwn = (o, k) => Object.hasOwn(o, k);
    const tipObject = (v) => v !== null && typeof v === 'object' && !Array.isArray(v);
    const tipText = (v) => typeof v === 'string' && v.trim() !== '';
    const tipSaid = new Set();
    const note = (key, message) => {
      if (tipSaid.has(key)) return;
      tipSaid.add(key);
      console.warn(`${NAMES.who}: ${message}`);
    };

    const own = new WeakMap();
    const handles = new Set();
    const inner = new WeakMap();
    const tidyBlocks = (name, value) => {
      const list = Array.isArray(value) ? value : tipObject(value) ? [value] : null;
      if (!list) {
        note(`tpl|${name}`, `tooltip template "${name}" must be a list of blocks; it is left out.`);
        return null;
      }
      return list.filter((b, i) => {
        if (!tipObject(b) || typeof b.type !== 'string') {
          note(`blk|${name}|${i}`, `tooltip template "${name}" block ${i + 1} needs a "type"; it is left out.`);
          return false;
        }
        if (!renderers.has(b.type)) {
          const later = LATER.includes(b.type) ? ' (it comes with a later kit)' : '';
          note(
            `blk|${name}|${i}`,
            `tooltip template "${name}" block ${i + 1}: "${b.type}" is not a block${later}; it takes ${[...renderers.keys()].join(', ')}.`,
          );
          return false;
        }
        return true;
      });
    };
    function placementWord(v, where) {
      const word = typeof v === 'string' ? v.trim() : '';
      if (PLACEMENTS.includes(word)) return word;
      note(
        `placement|${String(v)}`,
        `${where}: placement "${String(v)}" is not one of ${PLACEMENTS.join(', ')}; it reads as auto.`,
      );
      return 'auto';
    }
    function tooltip(settings, el) {
      const at = el ?? document.documentElement;
      if (settings !== undefined && settings !== null) {
        if (!tipObject(settings))
          note(
            'settings',
            'tooltip: give { tooltip, tooltip_template, tooltip_templates, tooltip_placement } (or nothing to read the setting).',
          );
        else {
          const mine = own.get(at) ?? {};
          if (tipOwn(settings, 'tooltip')) {
            const v = settings.tooltip;
            if (v === null || v === undefined) delete mine.tooltip;
            else if (typeof v === 'boolean') mine.tooltip = v;
            else note(`tooltip|${String(v)}`, 'tooltip: "tooltip" must be true or false; kept the old value.');
          }
          if (tipOwn(settings, 'tooltip_template')) {
            const v = settings.tooltip_template;
            if (v === null || v === undefined) delete mine.tooltip_template;
            else if (tipText(v)) mine.tooltip_template = v;
            else
              note(
                `template|${String(v)}`,
                'tooltip: "tooltip_template" must be a template\'s name; kept the old value.',
              );
          }
          if (tipOwn(settings, 'tooltip_placement')) {
            const v = settings.tooltip_placement;
            if (v === null || v === undefined) delete mine.placement;
            else mine.placement = placementWord(v, 'tooltip');
          }
          if (tipOwn(settings, 'tooltip_templates')) {
            const v = settings.tooltip_templates;
            if (v === null || v === undefined) delete mine.templates;
            else if (Array.isArray(v) && !v.length) mine.templates = new Map();
            else if (!tipObject(v))
              note('templates', 'tooltip: "tooltip_templates" must be { name: [blocks] }; kept the old value.');
            else {
              mine.templates = new Map();
              for (const [name, blocks] of Object.entries(v)) {
                const tidy = tidyBlocks(name, blocks);
                if (tidy) mine.templates.set(name, tidy);
              }
            }
          }
          own.set(at, mine);
          for (const h of handles) if (h.under(at)) h.redescribe();
        }
      }
      const got = settingsAt(at);
      const templates = {};
      for (let node = at; node; node = node.parentElement) {
        for (const [name, blocks] of own.get(node)?.templates ?? [])
          if (!tipOwn(templates, name)) templates[name] = blocks;
      }
      return {
        tooltip: got.tooltip,
        tooltip_template: got.tooltip_template,
        tooltip_templates: templates,
        tooltip_placement: got.placement,
      };
    }
    function settingsAt(el) {
      let on = null;
      let name = null;
      let namedAt = null;
      let placed = null;
      for (let node = el; node && (on === null || name === null || placed === null); node = node.parentElement) {
        const mine = own.get(node);
        if (!mine) continue;
        if (placed === null && tipOwn(mine, 'placement')) placed = mine.placement;
        if (on === null && tipOwn(mine, 'tooltip')) on = mine.tooltip;
        if (name === null && tipOwn(mine, 'tooltip_template')) {
          name = mine.tooltip_template;
          namedAt = node;
        }
      }
      const template = (wanted) => {
        for (let node = el; node; node = node.parentElement) {
          const found = own.get(node)?.templates?.get(wanted);
          if (found) return found;
        }
        return null;
      };
      return {
        tooltip: on !== false,
        tooltip_template: name,
        named_at: namedAt,
        template,
        placement: placed ?? 'auto',
      };
    }

    const recordOf = (rec) => ({
      tip_index: rec.index + 1,
      tip_id: String(rec.id),
      ...(tipObject(rec.row) ? rec.row : {}),
    });
    function contentOf(rec, at, { over = null, label = false } = {}) {
      if (!rec) return null;
      const reason = tipText(rec.reason) ? rec.reason : null;
      const row = tipObject(rec.row) ? rec.row : {};
      let blocks = null;
      let literal = false;
      const templated = rec.row !== null;
      if (at.tooltip && Array.isArray(over) && over.length) blocks = over;
      else if (at.tooltip && templated) {
        const t = row.tooltip;
        if (typeof t === 'string' && t.trim() !== '') {
          blocks = [{ type: 'text', text: t }];
          literal = true;
        } else if (typeof t === 'number' && Number.isFinite(t)) {
          blocks = [{ type: 'text', text: String(env.num(t)) }];
          literal = true;
        } else if (t !== null && t !== undefined && typeof t !== 'string' && typeof t !== 'number') {
          note(
            `row-tooltip|${rec.id}`,
            `the tooltip of ${String(rec.id)} must be text (a list of blocks comes with a later kit); it has none.`,
          );
        }
        const named = rec.own_template_only && !at.own_template ? null : at.tooltip_template;
        if (!blocks && named) {
          const found = at.template(named);
          if (found) blocks = found;
          else note(`no-template|${named}`, `tooltip template "${named}" is not defined; no tooltip is drawn from it.`);
        }
      }
      const first = label && (tipText(rec.label) || Number.isFinite(rec.label)) ? String(rec.label) : '';
      if (!blocks && !reason && !first) return null;
      return {
        blocks: blocks ?? [],
        literal,
        record: recordOf(rec),
        reason,
        rec,
        label: first,
        prefix: rec.reason_prefix === true,
      };
    }

    function valueWords(v, filter, field) {
      if (v === null || v === undefined || v === '') return DASH;
      if (typeof v === 'number') {
        if (!Number.isFinite(v)) return DASH;
        if (!filter) return env.format(v);
        return env.format(v, { digits: filter.digits, percent: filter.name === 'percent' });
      }
      if (typeof v === 'string') {
        if (filter) note(`filter-text|${field}`, `"${field}" is text, so its ${filter.name} filter is skipped.`);
        return v;
      }
      if (typeof v === 'boolean') return String(v);
      note(`value|${field}`, `"${field}" holds a list or an object; a tip shows a dash for it.`);
      return DASH;
    }
    const PH = { open: '{{', close: '}}', filter: '|', arg: ':' };
    const FILTERS = {
      number: { required: false, pattern: /^[0-9]+$/, min: 0, max: 100 },
      percent: { required: false, pattern: /^[0-9]+$/, min: 0, max: 100 },
    };
    function filterOf(word, shown) {
      const at = word.indexOf(PH.arg);
      const name = (at < 0 ? word : word.slice(0, at)).trim();
      const arg = at < 0 ? '' : word.slice(at + 1).trim();
      const spec = tipOwn(FILTERS, name) ? FILTERS[name] : null;
      let bad = word.includes(PH.filter) || !spec;
      if (!bad && (at >= 0 || spec.required)) {
        bad = !spec.pattern.test(arg) || Number(arg) < spec.min || Number(arg) > spec.max;
      }
      if (bad) {
        note(`filter|${shown}`, `the tooltip placeholder ${shown} has a filter it can't use; it shows as written.`);
        return null;
      }
      return { name, digits: at < 0 ? null : Number(arg) };
    }
    function expand(text, record) {
      const pieces = [];
      const all = String(text);
      let pos = 0;
      for (;;) {
        const start = all.indexOf(PH.open, pos);
        if (start < 0) break;
        const end = all.indexOf(PH.close, start + PH.open.length);
        if (end < 0) {
          note(`open|${all}`, `a tooltip placeholder "${PH.open}" never closes in "${all}"; it shows as typed.`);
          break;
        }
        if (start > pos) pieces.push({ text: all.slice(pos, start) });
        const inner = all.slice(start + PH.open.length, end);
        const shown = PH.open + inner + PH.close;
        const bar = inner.indexOf(PH.filter);
        const field = (bar < 0 ? inner : inner.slice(0, bar)).trim();
        const filter = bar < 0 ? undefined : filterOf(inner.slice(bar + 1).trim(), shown);
        if (!field || field.includes(PH.open) || filter === null) {
          note(`field|${shown}`, `the tooltip placeholder ${shown} names no field it can show; it shows as written.`);
          pieces.push({ value: shown });
        } else if (!tipOwn(record, field)) {
          note(`field|${field}`, `the tooltip placeholder "${field}" is not in the record; it shows a dash.`);
          pieces.push({ value: DASH });
        } else pieces.push({ value: valueWords(record[field], filter ?? null, field) });
        pos = end + PH.close.length;
      }
      if (pos < all.length) pieces.push({ text: all.slice(pos) });
      return pieces;
    }
    const asMarkdown = (pieces) =>
      pieces.map((p) => (p.text !== undefined ? p.text : env.markdown.escape(p.value))).join('');
    const asWords = (pieces) => pieces.map((p) => (p.text !== undefined ? p.text : p.value)).join('');
    const wordsOf = (content, text) =>
      content.literal ? String(text ?? '') : asWords(expand(text ?? '', content.record));
    const markdownOf = (content, text) =>
      content.literal ? String(text ?? '') : asMarkdown(expand(text ?? '', content.record));

    function codeOf(content, block) {
      if (!tipText(block.field)) return null;
      if (!tipOwn(content.record, block.field)) {
        note(
          `code|${block.field}`,
          `the tooltip code field "${block.field}" is not in the record; the block is left out.`,
        );
        return null;
      }
      const v = content.record[block.field];
      return typeof v === 'string' && v !== '' ? v : null;
    }

    function plainOf(content, withReason = true) {
      const out = [content.label ?? ''];
      const order = ['title', 'text', 'badge', 'fields', 'code'];
      const blocks = [...content.blocks].sort((a, b) => order.indexOf(a.type) - order.indexOf(b.type));
      for (const b of blocks) {
        if (b.type === 'title' || b.type === 'badge') out.push(wordsOf(content, b.text));
        else if (b.type === 'text') out.push(env.markdown.plain(markdownOf(content, b.text)));
        else if (b.type === 'fields') {
          for (const f of Array.isArray(b.fields) ? b.fields : []) {
            if (tipObject(f)) out.push(`${wordsOf(content, f.label)} ${wordsOf(content, f.value)}`.trim());
          }
        } else if (b.type === 'code') {
          const code = codeOf(content, b);
          if (code !== null) {
            const lines = code.split(/\r\n|\r|\n/);
            const n = lines.length;
            const said = (lines.find((l) => l.trim() !== '') ?? '').trim();
            const first = said.length > 120 ? `${said.slice(0, 120).trimEnd()}…` : said;
            if (n === 1) out.push(first ? `code: ${first}` : 'code, 1 line');
            else out.push(`code, ${n} lines${first ? `, starting ${first}` : ''}`);
          }
        }
      }
      if (withReason && content.reason) out.push(reasonWords(content));
      return out.filter((w) => w !== '').join(' ');
    }
    const reasonWords = (content) =>
      !content.reason
        ? ''
        : content.blocks.length || content.prefix
          ? `Not available: ${content.reason}`
          : content.reason;
    const titleOf = (content) => {
      const t = content.blocks.find((b) => b.type === 'title');
      return t ? wordsOf(content, t.text) : '';
    };
    const cut = (text) => (text.length > CUT ? `${text.slice(0, CUT)}…` : text);

    const el = (doc, tag, className, text) => {
      const node = doc.createElement(tag);
      if (className) node.className = className;
      if (text !== undefined) node.textContent = text;
      return node;
    };
    const boxes = new WeakMap();
    function boxesOf(doc) {
      if (boxes.has(doc)) return boxes.get(doc);
      const hint = el(doc, 'div', CSS);
      hint.setAttribute('role', 'tooltip');
      hint.setAttribute('aria-hidden', 'true'); // its words are the item's description, made at draw
      hint.setAttribute('data-hovercraft-keep', '');
      hint.hidden = true;
      const pin = el(doc, 'div', `${CSS} ${CSS}-pin`);
      pin.setAttribute('role', 'dialog');
      pin.setAttribute('tabindex', '-1');
      pin.setAttribute('data-hovercraft-keep', '');
      pin.hidden = true;
      for (const b of [hint, pin]) b.setAttribute(`${NAMES.attr}-box`, '');
      const live = el(doc, 'div', `${CSS}-live`);
      live.setAttribute('role', 'status');
      live.setAttribute('aria-live', 'polite');
      live.setAttribute('data-hovercraft-keep', '');
      const view = doc.defaultView;
      const sizes =
        typeof view?.ResizeObserver === 'function'
          ? new view.ResizeObserver(() => {
              const page = pageOf(doc);
              page.hint?.place();
              page.pin?.place();
            })
          : null;
      const made = {
        hint,
        pin,
        live,
        sizes,
        hintMotion: env.motion(hint, { axis: 'fade', rise: 0, exit: 'none' }),
        pinMotion: env.motion(pin, { axis: 'fade', rise: 0, exit: 'none' }),
      };
      boxes.set(doc, made);
      return made;
    }
    function modeOf(anchor) {
      let mode = anchor.closest('[data-hovercraft-mode]')?.getAttribute('data-hovercraft-mode') ?? 'light';
      if (mode !== 'light' && mode !== 'dark') {
        mode = anchor.ownerDocument.defaultView?.matchMedia?.('(prefers-color-scheme: dark)').matches
          ? 'dark'
          : 'light';
      }
      return mode;
    }
    function mountNear(box, anchor) {
      const doc = anchor.ownerDocument;
      const host = anchor.closest(MODAL) ?? doc.body;
      if (box.parentNode !== host) host.append(box);
      copyMarks(box, anchor);
    }
    function copyMarks(box, anchor) {
      for (const mark of MARKS) {
        const from = anchor.closest(`[${mark}]`);
        if (from) box.setAttribute(mark, from.getAttribute(mark));
        else box.removeAttribute(mark);
      }
      if (!NAMES.standalone) box.setAttribute('data-hovercraft-mode', modeOf(anchor));
    }
    function say(doc, words, near = null) {
      const { live } = boxesOf(doc);
      const host = near?.closest?.(MODAL) ?? doc.body;
      if (live.parentNode !== host) host.append(live);
      live.textContent = '';
      setTimeout(() => {
        live.textContent = words;
      }, 50);
    }
    function place(box, anchor, side, x = null, beside = null, line = null, isPin = false, placement = 'auto') {
      const doc = anchor.ownerDocument;
      const r = anchor.getBoundingClientRect();
      const vw = doc.documentElement.clientWidth;
      const vh = doc.documentElement.clientHeight;
      const probe = line && x !== null ? line : anchor;
      const pr = probe === anchor ? r : probe.getBoundingClientRect();
      const away =
        !probe.isConnected ||
        !probe.getClientRects().length ||
        doc.defaultView?.getComputedStyle(probe).visibility === 'hidden' ||
        pr.bottom < 0 ||
        pr.top > vh ||
        pr.right < 0 ||
        pr.left > vw;
      box.style.visibility = away ? 'hidden' : '';
      if (away) return;
      if (placement !== 'auto' && vw >= PHONE && placeOn(box, anchor, placement, x, line, isPin)) return;
      const w = box.offsetWidth;
      const h = box.offsetHeight;
      const part = beside?.getBoundingClientRect();
      const rtl = part && doc.defaultView?.getComputedStyle(beside).direction === 'rtl';
      const fitsRight = part && part.right + GAP + w <= vw - 4;
      const fitsLeft = part && part.left - GAP - w >= 4;
      const room = !part
        ? null
        : rtl
          ? fitsLeft
            ? 'left'
            : fitsRight
              ? 'right'
              : null
          : fitsRight
            ? 'right'
            : fitsLeft
              ? 'left'
              : null;
      if (isPin && !room && side !== 'right' && side !== 'left') {
        if (vw < 600) {
          box.dataset.side = 'sheet';
          box.style.left = '';
          box.style.top = '';
          return;
        }
        const row = (line ?? anchor).getBoundingClientRect();
        const edge = part ?? row;
        let left = rtl ? edge.left : edge.right - w;
        let top = row.bottom + GAP;
        box.dataset.side = 'below';
        if (top + h > vh - 4) {
          top = row.top - h - GAP;
          box.dataset.side = 'above';
        }
        left = Math.max(4, Math.min(left, vw - w - 4));
        top = Math.max(4, Math.min(top, vh - h - 4));
        box.style.left = `${Math.round(left)}px`;
        box.style.top = `${Math.round(top)}px`;
        return;
      }
      if (line && !isPin && !room) {
        const row = line.getBoundingClientRect();
        const rtl = doc.defaultView?.getComputedStyle(line).direction === 'rtl';
        const gap = x === null ? GAP : 14;
        const from = x === null ? (rtl ? r.left : r.right) : x;
        const ends = rtl ? from - gap - w : from + gap;
        const starts = rtl ? from + gap : from - gap - w;
        const fits = (at) => at >= 4 && at + w <= vw - 4;
        const at = fits(ends) ? ends : fits(starts) ? starts : null;
        if (at !== null) {
          box.dataset.side = at >= from ? 'right' : 'left';
          box.style.left = `${Math.round(at)}px`;
          box.style.top = `${Math.round(Math.max(4, Math.min(row.top, vh - h - 4)))}px`;
          return;
        }
      }
      const aside = side === 'right' || side === 'left';
      let left =
        side === 'right'
          ? r.right + GAP + 2
          : side === 'left'
            ? r.left - GAP - 2 - w
            : room === 'right'
              ? part.right + GAP
              : room === 'left'
                ? part.left - GAP - w
                : x === null
                  ? doc.defaultView?.getComputedStyle(anchor).direction === 'rtl'
                    ? r.right - w
                    : r.left
                  : x - w / 2;
      let top = aside ? r.top + (r.height - h) / 2 : room ? r.top : r.bottom + GAP;
      box.dataset.side = aside ? side : (room ?? 'below');
      if (!aside && !room && top + h > vh - 4) {
        top = r.top - h - GAP;
        box.dataset.side = 'above';
      }
      left = Math.max(4, Math.min(left, vw - w - 4));
      top = Math.max(4, Math.min(top, vh - h - 4));
      box.style.left = `${Math.round(left)}px`;
      box.style.top = `${Math.round(top)}px`;
    }

    function placeOn(box, anchor, want, x, line, isPin) {
      const doc = anchor.ownerDocument;
      const vw = doc.documentElement.clientWidth;
      const vh = doc.documentElement.clientHeight;
      const w = box.offsetWidth;
      const h = box.offsetHeight;
      const r = anchor.getBoundingClientRect();
      const row = line ? line.getBoundingClientRect() : null;
      const at = isPin ? null : x;
      const t = row
        ? {
            left: at ?? (isPin ? row.left : r.left),
            right: at ?? (isPin ? row.right : r.right),
            top: row.top,
            bottom: row.bottom,
          }
        : r;
      const rtl = doc.defaultView?.getComputedStyle(line ?? anchor).direction === 'rtl';
      const swap = { left: 'right', right: 'left' };
      const first = rtl && swap[want] ? swap[want] : want;
      const opposite = { top: 'bottom', bottom: 'top', left: 'right', right: 'left' };
      const gapX = at === null ? GAP + 2 : 14;
      const midX = (t.left + t.right) / 2 - w / 2;
      const midY = (t.top + t.bottom) / 2 - h / 2;
      const spots = {
        top: { left: midX, top: t.top - GAP - h, fits: t.top - GAP - h >= 4, side: 'above' },
        bottom: { left: midX, top: t.bottom + GAP, fits: t.bottom + GAP + h <= vh - 4, side: 'below' },
        left: { left: t.left - gapX - w, top: midY, fits: t.left - gapX - w >= 4, side: 'left' },
        right: { left: t.right + gapX, top: midY, fits: t.right + gapX + w <= vw - 4, side: 'right' },
      };
      for (const name of [first, opposite[first]]) {
        const at = spots[name];
        if (!at.fits) continue;
        box.dataset.side = at.side;
        box.style.left = `${Math.round(Math.max(4, Math.min(at.left, vw - w - 4)))}px`;
        box.style.top = `${Math.round(Math.max(4, Math.min(at.top, vh - h - 4)))}px`;
        return true;
      }
      return false;
    }

    function room(box, open) {
      const doc = box.ownerDocument;
      const view = doc.defaultView;
      const page = doc.scrollingElement ?? doc.documentElement;
      let pad = doc.querySelector(`.${CSS}-room`);
      const at = (y) => {
        pad.style.top = `${Math.max(0, Math.round(y) - 1)}px`;
      };
      if (!open) {
        if (!pad || pad.dataset.closing) return;
        const bottom = view.scrollY + page.clientHeight;
        if (bottom <= pad.base) {
          pad.remove();
          return;
        }
        pad.dataset.closing = 'true';
        at(bottom);
        const gone = () => {
          if (view.scrollY + page.clientHeight > pad.base + 1) return;
          view.removeEventListener('scroll', gone);
          pad.remove();
        };
        view.addEventListener('scroll', gone);
        return;
      }
      if (pad?.dataset.closing) {
        pad.remove();
        pad = null;
      }
      if (!pad) {
        pad = el(doc, 'div', `${CSS}-room`);
        pad.setAttribute('aria-hidden', 'true');
        pad.setAttribute('data-hovercraft-keep', '');
        pad.base = page.scrollHeight;
        doc.body.append(pad);
      }
      at(pad.base + box.offsetHeight + GAP);
    }
    function reveal(item, box) {
      const vh = item.ownerDocument.documentElement.clientHeight;
      if (item.getBoundingClientRect().bottom <= vh - box.offsetHeight - GAP) return;
      const was = item.style.scrollMarginBlockEnd;
      item.style.scrollMarginBlockEnd = `${box.offsetHeight + GAP}px`;
      item.scrollIntoView({ block: 'end', inline: 'nearest' });
      item.style.scrollMarginBlockEnd = was;
    }

    const renderers = new Map();
    renderers.set('title', (b, _record, box, { content, pinned }) =>
      pinned ? null : { node: el(box.ownerDocument, 'div', `${CSS}-title`, wordsOf(content, b.text)) },
    );
    renderers.set('text', (b, _record, box, { content }) => {
      const words = box.appendChild(el(box.ownerDocument, 'div', `${CSS}-text`));
      env.markdown(words, markdownOf(content, b.text), { field: 'tooltip', id: content.rec.id });
      return { node: words };
    });
    renderers.set('badge', (b, _record, box, { content }) => {
      const badge = el(box.ownerDocument, 'span', `${CSS}-badge`, wordsOf(content, b.text));
      if (tipText(b.tone)) badge.dataset.tone = b.tone;
      return { node: badge };
    });
    renderers.set('fields', (b, _record, box, { content }) => {
      const doc = box.ownerDocument;
      const dl = el(doc, 'dl', `${CSS}-fields`);
      for (const f of Array.isArray(b.fields) ? b.fields : []) {
        if (!tipObject(f)) continue;
        const row = el(doc, 'div');
        row.append(el(doc, 'dt', '', wordsOf(content, f.label)), el(doc, 'dd', '', wordsOf(content, f.value)));
        dl.append(row);
      }
      return { node: dl };
    });
    renderers.set('code', (b, _record, box, { content, pinned, onCopy, code: at }) => {
      const doc = box.ownerDocument;
      const code = codeOf(content, b);
      if (code === null) return null;
      const wrap = el(doc, 'div', `${CSS}-code`);
      const pre = el(doc, 'pre');
      const inner = el(doc, 'code', '', code);
      if (tipText(b.language)) inner.dataset.language = b.language;
      pre.append(inner);
      wrap.append(pre);
      if (pinned) {
        const cue = () => {
          const room = pre.scrollWidth - pre.clientWidth;
          const rtl = doc.defaultView?.getComputedStyle(pre).direction === 'rtl';
          const left = pre.scrollLeft - (rtl ? -room : 0);
          const right = (rtl ? 0 : room) - pre.scrollLeft;
          wrap.classList.toggle(`${CSS}-code-more-right`, room > 1 && right > 1);
          wrap.classList.toggle(`${CSS}-code-more-left`, room > 1 && left > 1);
        };
        pre.addEventListener('scroll', cue, { passive: true });
        const View = doc.defaultView;
        if (typeof View?.ResizeObserver === 'function') new View.ResizeObserver(cue).observe(pre);
      }
      if (pinned && b.copy === true) {
        const bar = el(doc, 'div', `${CSS}-copybar`);
        const button = el(doc, 'button', `${CSS}-copy`, 'Copy');
        button.type = 'button';
        const status = el(doc, 'span', `${CSS}-status`);
        status.setAttribute('role', 'status');
        button.addEventListener('click', (e) => onCopy(code, b.field, at, status, e.detail === 0 ? 'key' : 'click'));
        bar.append(button, status);
        wrap.append(bar);
      }
      return { node: wrap };
    });

    const lives = new WeakMap();
    const tryCall = (fn, ...args) => {
      try {
        fn?.(...args);
      } catch (e) {
        note(`life|${e?.message}`, `a tooltip block failed to draw or let go: ${e?.message ?? e}`);
      }
    };
    function endLives(box, info) {
      for (const h of lives.get(box) ?? []) {
        tryCall(h.release, info);
        tryCall(h.destroy);
      }
      lives.set(box, []);
      box.classList.remove(`${CSS}-wide`);
    }
    function startLives(box) {
      for (const h of lives.get(box) ?? []) {
        if (h.mount && !h.mounted) {
          h.mounted = true;
          tryCall(h.mount);
        }
      }
    }
    function fill(box, content, { pinned, onCopy }) {
      const doc = box.ownerDocument;
      endLives(box, { reason: 'redraw', by: 'data', via: null });
      box.replaceChildren();
      if (content.label) box.append(el(doc, 'div', `${CSS}-title`, content.label));
      if (!content.blocks.length && content.reason) {
        box.append(el(doc, 'div', `${CSS}-line`, reasonWords(content)));
        return;
      }
      if (pinned) {
        const title = titleOf(content);
        const close = el(doc, 'button', `${CSS}-close`, '×');
        close.type = 'button';
        close.setAttribute('aria-label', 'Close');
        if (title) {
          const head = el(doc, 'div', `${CSS}-head`);
          head.append(el(doc, 'div', `${CSS}-title`, title), close);
          box.append(head);
        } else {
          close.classList.add(`${CSS}-corner`);
          box.append(close);
        }
      }
      const kept = [];
      let code = -1;
      for (const b of content.blocks) {
        if (b.type === 'code') code += 1;
        const render = renderers.get(b.type);
        if (!render) continue;
        let h = null;
        try {
          h = render(b, content.record, box, { content, pinned, onCopy, code });
        } catch (e) {
          note(`render|${b.type}`, `a tooltip ${b.type} block could not be drawn: ${e?.message ?? e}`);
        }
        if (!h) continue;
        if (h.node && h.node.parentNode !== box) box.append(h.node);
        if (h.mount || h.release || h.destroy) kept.push(h);
      }
      lives.set(box, kept);
      if (kept.some((h) => h.mount)) box.classList.add(`${CSS}-wide`);
      if (content.reason) box.append(el(doc, 'div', `${CSS}-line`, reasonWords(content)));
    }

    function copyText(doc, text, near, done) {
      const nav = doc.defaultView?.navigator;
      if (nav?.clipboard?.writeText && doc.defaultView.isSecureContext) {
        nav.clipboard.writeText(text).then(
          () => done(true),
          () => done(copyOld(doc, text, near)),
        );
      } else done(copyOld(doc, text, near));
    }
    function copyOld(doc, text, near) {
      const had = doc.activeElement;
      const area = el(doc, 'textarea', `${CSS}-copying`);
      area.value = text;
      area.setAttribute('readonly', '');
      (near ?? doc.body).append(area);
      area.select();
      let ok = false;
      try {
        ok = doc.execCommand('copy');
      } catch {
        ok = false;
      }
      area.remove();
      if (had?.isConnected) had.focus({ preventScroll: true });
      return ok;
    }

    const pages = new WeakMap();
    function pageOf(doc) {
      if (!pages.has(doc)) pages.set(doc, { hint: null, pin: null, keys: false, skip: false, skipTimer: 0 });
      return pages.get(doc);
    }
    const focusables = (box) => [...box.querySelectorAll('button, a[href]')].filter((n) => !n.hidden && !n.disabled);
    const stopsOff = (box) => {
      for (const n of focusables(box)) if (n.tabIndex !== -1) n.tabIndex = -1;
    };
    const TABBABLE = 'a[href], button, input, select, textarea, summary, [tabindex]';
    const FOCUSABLE =
      'a[href], button, input, select, textarea, summary, [tabindex], [contenteditable]:not([contenteditable="false"])';
    const STOP =
      'a[href], button:not([disabled]), [role="tab"], [role="button"], [role="link"], [role="menuitem"], [role="option"], [tabindex]:not([tabindex^="-"])';
    const NOT_STOP =
      'dialog, main, nav, aside, section, form, [role="dialog"], [role="alertdialog"], [role="main"], [role="navigation"], [role="region"], [role="tabpanel"], [role="complementary"], [role="form"], [role="search"]';
    const labelOf = (n) => {
      const copy = n.cloneNode(true);
      for (const d of copy.querySelectorAll(`.${CSS}-desc`)) d.remove();
      return copy.textContent.trim();
    };
    const focusRoot = (target) => {
      if (target === target.ownerDocument.body) {
        target.ownerDocument.activeElement?.blur?.();
        return;
      }
      const added = !target.hasAttribute('tabindex');
      if (added) target.setAttribute('tabindex', '-1');
      target.focus({ preventScroll: true });
      if (added) target.addEventListener('blur', () => target.removeAttribute('tabindex'), { once: true });
    };
    function nextAfter(doc, part) {
      const scope = part.closest(MODAL) ?? doc;
      const all = [...scope.querySelectorAll(TABBABLE)].filter(
        (n) =>
          !part.contains(n) &&
          !n.closest(`.${CSS}`) &&
          n.tabIndex >= 0 &&
          !n.disabled &&
          n.getClientRects().length &&
          !n.closest('[inert], [hidden]'),
      );
      const after = all.find((n) => part.compareDocumentPosition(n) & Node.DOCUMENT_POSITION_FOLLOWING);
      return after ?? (scope === doc ? null : (all[0] ?? null));
    }

    function listenPage(doc) {
      const page = pageOf(doc);
      if (page.keys) return;
      page.keys = true;
      const view = doc.defaultView;
      const onKey = (e) => {
        if (e.isComposing || e.keyCode === 229) return;
        if (e.key === 'Escape' && page.hint) {
          page.hint.handle.hideHint();
          e.preventDefault();
          e.stopPropagation();
        } else if (e.key === 'Tab' && e.shiftKey && page.pin && doc.activeElement === page.pin.after()) {
          const stops = focusables(page.pin.box);
          if (!stops.length) return;
          e.preventDefault();
          e.stopPropagation();
          stops.at(-1).focus();
        }
      };
      const onMove = () => {
        if (page.hint) page.hint.place();
        if (page.pin) page.pin.place();
      };
      const onEscape = (e) => {
        if (e.key !== 'Escape' || e.defaultPrevented || e.isComposing || !page.pin) return;
        const modal = doc.activeElement?.closest?.(MODAL);
        if (modal && !modal.contains(page.pin.box)) return;
        page.pin.handle.closePin({ reason: 'escape', by: 'reader', via: 'key' }, true);
        e.preventDefault();
      };
      view.addEventListener('keydown', onKey, true);
      view.addEventListener('keydown', onEscape);
      doc.addEventListener('scroll', onMove, true);
      view.addEventListener('resize', onMove);
      doc.addEventListener('report:tab', () => page.pin?.check('reader'));
    }

    const reasonOf = (n) =>
      n.getAttribute('aria-disabled') === 'true' && tipText(n.dataset.reason)
        ? {
            id: n.dataset.id ?? '',
            kind: 'row',
            label: labelOf(n),
            row: null,
            index: 0,
            reason: n.dataset.reason,
          }
        : null;
    function tips(
      host,
      {
        box,
        selector,
        record = reasonOf,
        content = null,
        pin = false,
        when = () => true,
        side = 'below',
        placement = 'auto',
        anchor = null,
        focusAt = null,
        align = 'item',
        label = false,
        look = null,
        roles = [],
        beside = false,
        root = null,
      } = {},
    ) {
      const doc = box.ownerDocument;
      const page = pageOf(doc);
      const owner = root ?? host?.root ?? box;
      const kit = boxesOf(doc);
      const canPin = (n) => !disabledAt(n) && (typeof pin === 'function' ? pin(n) : pin) === true;
      const sideOf = () => (typeof side === 'function' ? side() : side);
      let timer = 0;
      let leave = 0;
      let awayOff = null;
      let seenHint = null;
      let inHint = false;
      let lastPointer = 'mouse';
      let hovered = false;
      let gone = false;
      let pointerX = null;
      const uidBase = `${CSS}-${numberOf(owner)}`;
      let made = 0;

      const items = () => [...box.querySelectorAll(selector)].filter((n) => box.contains(n));
      const itemOf = (target) => {
        const n = target?.closest?.(selector);
        return n && box.contains(n) ? n : null;
      };
      const recOf = (n) => {
        try {
          return record(n);
        } catch (e) {
          note(`record|${selector}`, `a part's tip record threw; that item has no tip. ${e?.message ?? ''}`);
          return null;
        }
      };
      const byId = (id) => items().find((n) => String(recOf(n)?.id) === String(id)) ?? null;
      const overOf = (n) => {
        if (typeof content !== 'function') return null;
        try {
          return content(n);
        } catch {
          return null;
        }
      };
      const settingsFor = (n) => {
        const at = settingsAt(n);
        at.own_template = at.named_at !== null && (at.named_at === owner || owner.contains(at.named_at));
        return at;
      };
      const contentFor = (n) =>
        when(n)
          ? contentOf(recOf(n), settingsFor(n), {
              over: overOf(n),
              label: typeof label === 'function' ? label(n) : label,
            })
          : null;
      const PLACE_ATTR = `${NAMES.attr}-placement`;
      const placementOf = (n) => {
        for (let node = n; node && node.nodeType === 1; node = node.parentElement) {
          const attr = node.getAttribute(PLACE_ATTR);
          if (attr !== null) return placementWord(attr, PLACE_ATTR);
          const mine = own.get(node);
          if (mine && tipOwn(mine, 'placement')) return mine.placement;
        }
        const given = typeof placement === 'function' ? placement(n) : placement;
        return given === 'auto' || given === undefined || given === null ? 'auto' : placementWord(given, 'ui.tips');
      };
      const rootEl = () => (owner === doc.documentElement ? doc.body : owner);
      const anchorOf = (n) => (typeof anchor === 'function' ? anchor(n) : null) ?? focusOf(n);
      const focusOf = (n) => {
        const given = typeof focusAt === 'function' ? focusAt(n) : null;
        if (given) return given;
        if (n.matches(FOCUSABLE)) return n;
        const up = stopAround(n);
        return up && !several(up) ? up : n;
      };
      const stopAround = (n) => {
        const up = n.parentElement?.closest(`${STOP}, a`);
        return !up || up === box || up === owner || !box.contains(up) || up.matches(NOT_STOP) ? null : up;
      };
      const several = (up) => {
        let held = 0;
        for (const i of up.querySelectorAll(selector)) if (box.contains(i) && ++held > 1) return true;
        return false;
      };
      const sharedStop = (n) => {
        if ((typeof focusAt === 'function' && focusAt(n)) || n.matches(FOCUSABLE)) return null;
        const up = stopAround(n);
        return up && several(up) ? up : null;
      };
      const itemAt = (target) => {
        const n = itemOf(target);
        if (n || !target?.querySelectorAll) return n;
        return [...target.querySelectorAll(selector)].find((i) => box.contains(i) && focusOf(i) === target) ?? null;
      };
      const besideOf = (n) => (typeof beside === 'function' ? beside(n) : beside ? box : null);
      const pinBeside = (n) => (typeof beside === 'function' ? beside(n) : box);
      const atPointer = (n) => (typeof align === 'function' ? align(n) : align) === 'pointer';
      const xOf = (n) => (atPointer(n) ? pointerX : null);
      const looks = (b, from = null) => {
        if (look && b.dataset.look !== look) b.dataset.look = look;
        else if (!look && b.dataset.look) delete b.dataset.look;
        for (const role of roles) {
          const own = from ? (doc.defaultView?.getComputedStyle(from).getPropertyValue(role).trim() ?? '') : '';
          if (own) b.style.setProperty(role, own);
          else b.style.removeProperty(role);
        }
      };

      const DESC = `${CSS}-desc`;
      const describedAs = (n) =>
        (n.getAttribute('aria-describedby') ?? '')
          .split(/\s+/)
          .filter(Boolean)
          .map((id) => doc.getElementById(id)?.textContent ?? '');
      const VOID =
        'input, select, textarea, img, iframe, video, audio, canvas, svg, area, br, hr, embed, object, meter, progress';
      const mine = (n) =>
        (n.getAttribute('aria-describedby') ?? '').split(/\s+/).some((id) => id.startsWith(`${uidBase}-`));
      const addDesc = (n, words, first = false) => {
        const d = el(doc, 'span', DESC, words);
        d.hidden = true;
        made += 1;
        d.id = `${uidBase}-${made}`;
        if (n.matches(VOID)) n.after(d);
        else if (n.tagName === 'TR') (n.firstElementChild ?? n).append(d);
        else n.append(d);
        const was = n.getAttribute('aria-describedby');
        const now = was ? (first ? `${d.id} ${was}` : `${was} ${d.id}`) : d.id;
        if (now !== was) n.setAttribute('aria-describedby', now);
      };
      function describe() {
        for (const item of items()) {
          const n = focusOf(item);
          if (mine(n) || !when(item)) continue;
          const c = contentFor(item);
          if (!c) continue;
          const name = String(c.rec.label ?? '');
          const blocks = c.blocks.filter((b) => !(b.type === 'title' && name && wordsOf(c, b.text) === name));
          const words = c.blocks.length ? plainOf({ ...c, label: '', blocks }, false) : '';
          const why = c.prefix ? reasonWords(c) : c.reason;
          const shared = sharedStop(item);
          for (const at of shared ? [n, shared] : [n]) {
            const said = describedAs(at);
            if (words && !said.includes(words)) addDesc(at, words, !!why && said.includes(why));
            if (why && !said.includes(why)) addDesc(at, why);
          }
        }
        watch.takeRecords();
      }
      function undescribe() {
        for (const d of box.querySelectorAll(`.${DESC}`)) {
          if (!d.id.startsWith(`${uidBase}-`)) continue;
          const n = box.querySelector(`[aria-describedby~="${d.id}"]`);
          const was = n?.getAttribute('aria-describedby') ?? '';
          const now = was
            .split(/\s+/)
            .filter((id) => id && id !== d.id)
            .join(' ');
          if (n && now !== was) now ? n.setAttribute('aria-describedby', now) : n.removeAttribute('aria-describedby');
          d.remove();
        }
      }
      const watch = new MutationObserver((records) => {
        const ours = records.every(
          (r) => [...r.addedNodes].every((x) => x.classList?.contains(DESC)) && !r.removedNodes.length,
        );
        if (ours) return;
        describe();
        if (page.hint?.handle === handle && !page.hint.anchor.isConnected) hideHint();
        if (page.pin?.handle === handle) page.pin.check('data');
      });
      watch.observe(box, { childList: true, subtree: true });
      describe();

      function hideHint(moved = false) {
        clearTimeout(timer);
        clearTimeout(leave);
        timer = 0;
        awayOff?.();
        awayOff = null;
        seenHint?.disconnect();
        seenHint = null;
        inHint = false;
        if (page.hint?.handle !== handle) return;
        const delayed = page.hint.delayed;
        page.hint = null;
        if (moved !== true) closeSkip();
        kit.sizes?.unobserve(kit.hint);
        endLives(kit.hint, { reason: 'closed', by: 'reader', via: null });
        kit.hintMotion.set(false);
        if (moved === true && delayed) {
          page.skip = true;
          clearTimeout(page.skipTimer);
          page.skipTimer = setTimeout(() => {
            page.skip = false;
          }, HINT_DELAY);
        }
      }
      function closeSkip() {
        page.skip = false;
        clearTimeout(page.skipTimer);
      }
      function showHint(n, how) {
        if (page.pin?.handle === handle && page.pin.anchor() === n) return;
        const c = contentFor(n);
        if (!c) return;
        if (page.hint && page.hint.handle !== handle) page.hint.handle.hideHint();
        const a = anchorOf(n);
        mountNear(kit.hint, a);
        looks(kit.hint, a);
        fill(kit.hint, c, { pinned: false });
        const s = sideOf();
        const x = how === 'pointer' || how === 'tap' ? xOf(n) : null; // a tip shown by the keyboard is not where the mouse last was
        const by = besideOf(n);
        closeSkip();
        page.hint = {
          handle,
          anchor: n,
          how,
          delayed: c.blocks.length > 0,
          place: () => {
            copyMarks(kit.hint, anchorOf(n));
            place(kit.hint, anchorOf(n), s, x, by, atPointer(n) ? n : null, false, placementOf(n));
          },
        };
        kit.hintMotion.set(true);
        page.hint.place();
        startLives(kit.hint);
        kit.sizes?.observe(kit.hint);
        listenPage(doc);
        seenHint?.disconnect();
        const view = doc.defaultView;
        seenHint =
          typeof view?.IntersectionObserver === 'function'
            ? new view.IntersectionObserver(() => {
                if (page.hint?.handle !== handle || page.hint.anchor !== n) return;
                if (!n.isConnected || !n.getClientRects().length) hideHint();
              })
            : null;
        seenHint?.observe(n);
      }
      function soon(n, how) {
        clearTimeout(timer);
        clearTimeout(leave);
        const c = contentFor(n);
        if (!c) return;
        if (page.hint?.handle === handle && page.hint.anchor === n) return;
        const open = page.hint?.delayed === true;
        const wait = c.blocks.length && !page.skip && !open ? HINT_DELAY : 0;
        if (!wait) showHint(n, how);
        else timer = setTimeout(() => n.isConnected && showHint(n, how), wait);
      }
      const over = (e) => {
        if (e.pointerType === 'touch') return;
        const n = itemAt(e.target);
        if (!n) return;
        hovered = true;
        clearTimeout(leave);
        if (page.hint?.handle === handle && page.hint.anchor !== n) hideHint(true);
        soon(n, 'pointer');
      };
      const out = (e) => {
        if (e.pointerType === 'touch') return;
        if (e.pointerType === 'pen' && page.hint?.handle === handle && page.hint.how === 'tap') return;
        const n = itemAt(e.target);
        if (!n || n.contains(e.relatedTarget) || anchorOf(n).contains(e.relatedTarget)) return;
        hovered = false;
        clearTimeout(timer);
        if (kit.hint.contains(e.relatedTarget)) return;
        leave = setTimeout(() => hideHint(true), LEAVE_MS);
      };
      const hintOver = () => {
        if (page.hint?.handle !== handle) return;
        clearTimeout(leave);
        inHint = true;
      };
      const hintOut = (e) => {
        if (page.hint?.handle !== handle || kit.hint.contains(e.relatedTarget)) return;
        if (e.pointerType === 'touch' || page.hint.how === 'tap') return; // a tap's hint stays until a tap elsewhere
        const n = page.hint.anchor;
        if (n.contains(e.relatedTarget) || anchorOf(n).contains(e.relatedTarget)) return;
        inHint = false;
        if (e.buttons & 1) {
          const up = (ev) => {
            if (page.hint?.handle !== handle || kit.hint.contains(ev.target) || anchorOf(n).contains(ev.target)) return;
            const sel = doc.getSelection?.();
            if (sel && String(sel) && kit.hint.contains(sel.anchorNode)) {
              doc.addEventListener('pointerdown', (d) => !kit.hint.contains(d.target) && hideHint(true), {
                capture: true,
                once: true,
              });
            } else hideHint(true);
          };
          doc.addEventListener('pointerup', up, { capture: true, once: true });
          return;
        }
        leave = setTimeout(() => hideHint(true), LEAVE_MS);
      };
      const focus = (e) => {
        const n = itemAt(e.target);
        const stop = typeof focusAt !== 'function' || (n && e.target === focusOf(n));
        if (n && stop && e.target.matches(':focus-visible')) soon(n, 'focus');
        else if (!n) hideHint(true);
      };
      const blur = (e) => {
        clearTimeout(timer);
        if (page.hint?.handle !== handle) return;
        if (kit.hint.contains(e.relatedTarget) || inHint) return;
        if (hovered && page.hint.how === 'pointer') return; // the pointer still rests on it
        hideHint(true);
      };
      const down = (e) => {
        lastPointer = e.pointerType || 'mouse';
        pointerX = e.clientX;
      };
      const moved = (e) => {
        if (align !== 'item') pointerX = e.clientX;
      };
      function disabledAt(n) {
        return (
          !!n && (n.getAttribute('aria-disabled') === 'true' || focusOf(n)?.getAttribute('aria-disabled') === 'true')
        );
      }
      const ACTS =
        'a[href], button, input, select, textarea, summary, [role="button"], [role="tab"], [role="link"], [role="menuitem"], [role="menuitemcheckbox"], [role="menuitemradio"], [role="option"], [role="treeitem"], [role="checkbox"], [role="radio"], [role="switch"], [contenteditable]:not([contenteditable="false"])';
      const acts = (n) => {
        const control = n.closest(ACTS) ?? (focusOf(n).matches(ACTS) ? focusOf(n) : null);
        if (!control || n.getAttribute('aria-disabled') === 'true' || control.getAttribute('aria-disabled') === 'true')
          return false;
        return !control.matches(':disabled') && !control.closest('fieldset[disabled]');
      };
      const tap = (e) => {
        const n = itemAt(e.target);
        if (!n) return;
        const c = contentFor(n);
        if (!c) return;
        if (e.detail === 0) return;
        const reasonOnly = !c.blocks.length;
        const touched = lastPointer !== 'mouse'; // a finger or a pen; a mouse has its hover
        if (!reasonOnly && (!touched || canPin(n))) return;
        tapOn(n, touched);
      };
      function swallowClick() {
        let wait = 0;
        const off = () => {
          clearTimeout(wait);
          doc.removeEventListener('click', swallow, true);
          doc.removeEventListener('pointerdown', off, true);
        };
        const swallow = (e) => {
          e.stopPropagation();
          e.preventDefault();
          off();
        };
        doc.addEventListener('click', swallow, true);
        doc.addEventListener('pointerdown', off, true);
        wait = setTimeout(off, 700);
      }
      function tapOn(n, touched = false) {
        clearTimeout(timer);
        clearTimeout(leave);
        const shown = page.hint?.handle === handle && page.hint.anchor === n;
        if (!touched) {
          if (!shown) showHint(n, 'pointer');
          return;
        }
        if (shown && page.hint.how === 'tap') {
          if (!acts(n)) hideHint();
          return;
        }
        showHint(n, 'tap');
        if (page.hint?.handle !== handle || page.hint.anchor !== n) return;
        awayOff?.();
        const away = (ev) => {
          if (kit.hint.contains(ev.target)) {
            if (!focusables(kit.hint).length) {
              hideHint();
              swallowClick();
            }
            return;
          }
          if (itemAt(ev.target) === n) return;
          hideHint();
        };
        doc.addEventListener('pointerdown', away, true);
        awayOff = () => doc.removeEventListener('pointerdown', away, true);
      }

      const used = (e) => {
        e.preventDefault();
        e.stopPropagation();
      };
      const keys = (e) => {
        if (e.isComposing || e.keyCode === 229) return;
        if (e.key === 'Escape') {
          if (page.hint?.handle === handle) {
            hideHint();
            used(e);
          } else if (page.pin?.handle === handle && itemAt(doc.activeElement)) {
            closePin({ reason: 'escape', by: 'reader', via: 'key' });
            used(e);
          }
        } else if (e.key === 'Tab' && !e.shiftKey && page.pin?.handle === handle) {
          const stops = focusables(page.pin.box);
          if (stops.length && page.pin.focusAt() === doc.activeElement) {
            used(e);
            stops[0].focus();
          }
        }
      };
      const escapeInPart = (e) => {
        if (e.key !== 'Escape' || e.defaultPrevented || e.isComposing || e.keyCode === 229) return;
        if (page.pin?.handle !== handle) return;
        closePin({ reason: 'escape', by: 'reader', via: 'key' }, true);
        used(e);
      };
      const tipKeys = (e) => {
        if (page.pin?.handle !== handle || e.isComposing || e.keyCode === 229) return;
        if (e.key === 'Escape') {
          if (e.defaultPrevented) return;
          used(e);
          closePin({ reason: 'escape', by: 'reader', via: 'key' }, true);
        } else if (e.key === 'Tab') {
          const stops = focusables(page.pin.box);
          const at = stops.indexOf(doc.activeElement) + (e.shiftKey ? -1 : 1);
          const stop = () => owner.querySelector('[tabindex="0"]') ?? page.pin.focusAt();
          const to =
            at >= 0 && at < stops.length
              ? stops[at]
              : e.shiftKey
                ? page.pin.focusAt()
                : (page.pin.after() ?? (owner.closest(MODAL) ? stop() : null));
          if (!to) return;
          to.focus();
          if (doc.activeElement === to) used(e);
        }
      };

      const pinned = () => (page.pin?.handle === handle ? page.pin.id : null);
      function emitTip(state, id, kind, reason, text, info, from, item = null) {
        env.emit(
          host,
          'hovercraft:tip',
          {
            state,
            id,
            kind,
            ids: state === 'pinned' ? [id] : [],
            reason,
            text,
            by: info.by ?? 'reader',
            from,
            via: info.via ?? null,
          },
          item,
        );
      }
      function closePin(info = {}, refocus = false) {
        const p = page.pin;
        if (!p || p.handle !== handle) return false;
        const anchor = p.focusAt();
        page.pin = null;
        p.stop();
        const hadFocus = p.box.contains(doc.activeElement);
        kit.sizes?.unobserve(p.box);
        room(p.box, false);
        endLives(p.box, { reason: info.reason ?? 'host', by: info.by ?? 'reader', via: info.via ?? null });
        kit.pinMotion.set(false);
        emitTip('closed', p.id, p.kind, info.reason ?? 'host', p.text, info, p.id, p.item());
        if (refocus || hadFocus) {
          if (anchor?.isConnected) anchor.focus({ preventScroll: true });
          else if (hadFocus) {
            focusRoot(rootEl());
          }
        }
        return true;
      }
      function pinTo(id, info = {}, at = null) {
        const key = String(id);
        const n = at && box.contains(at) ? at : byId(key);
        if (!canPin(n)) {
          note(
            `pin|${selector}`,
            'a tip can pin only where picking is on and tooltip_pin is set; this one shows as a hint.',
          );
          return false;
        }
        if (pinned() === key) return true;
        const c = n ? contentFor(n) : null;
        if (!c?.blocks.length) return false;
        const from = pinned();
        const by = info.by ?? 'reader';
        if (page.pin) page.pin.handle.closePin({ reason: 'replaced', by, via: info.via ?? null });
        if (page.hint) page.hint.handle.hideHint();
        const rec = c.rec;
        const kind = rec.kind ?? 'row';
        const text = cut(plainOf(c));
        const name = titleOf(c) || String(rec.label ?? key);
        const s = sideOf();
        const x = info.via === 'click' ? xOf(n) : null; // a pin by key or by the host is not where the mouse last was
        const pbox = kit.pin;
        mountNear(pbox, anchorOf(n));
        looks(pbox, anchorOf(n));
        const onCopy = (code, field, block, status, via) =>
          copyText(doc, code, pbox, (ok) => {
            const item = page.pin?.item() ?? n;
            env.emit(host, 'hovercraft:copy', { id: key, kind, field, block, ok, by: 'reader', via }, item);
            status.textContent = '';
            clearTimeout(status.tipTimer);
            setTimeout(() => {
              status.textContent = ok ? 'Copied' : 'Copy failed';
              status.tipTimer = setTimeout(() => {
                status.textContent = '';
              }, 3000);
            }, 50);
          });
        const born = doc.defaultView?.event ?? null;
        fill(pbox, c, { pinned: true, onCopy });
        pbox.setAttribute('aria-label', name);
        const close = pbox.querySelector(`.${CSS}-close`);
        const onClose = (e) =>
          closePin({ reason: 'button', by: 'reader', via: e.detail === 0 ? 'key' : 'click' }, true);
        close?.addEventListener('click', onClose);
        const offOutside = env.outside(owner, (e) => {
          const hit = itemAt(e.target);
          if (e === born || pbox.contains(e.target) || (hit && canPin(hit))) return;
          closePin({ reason: 'outside', by: 'reader', via: 'click' });
        });
        const inside = (e) => {
          const hit = itemAt(e.target);
          if (e === born || (hit && canPin(hit)) || pbox.contains(e.target)) return;
          if (e.target.closest?.('[data-hovercraft-keep]')) return; // the part's own controls (a table's sort, search, paging)
          closePin({ reason: 'outside', by: 'reader', via: 'click' });
        };
        owner.addEventListener('click', inside);
        pbox.addEventListener('keydown', tipKeys);
        const around = n.closest('dialog');
        const entry = {
          owner,
          close: (i = {}) => closePin({ reason: 'below', by: i.by ?? 'reader', via: i.via ?? null }),
        };
        const stack = around ? env.layer(doc) : null;
        stack?.push(entry);
        const onAround = () => closePin({ reason: 'below', by: 'reader', via: null });
        around?.addEventListener('close', onAround);
        let anchorEl = n;
        let clipped = false;
        const view = doc.defaultView;
        const seen =
          typeof view?.IntersectionObserver === 'function'
            ? new view.IntersectionObserver((list) => {
                if (page.pin?.handle !== handle) return;
                const a = page.pin.anchor();
                if (!a?.getClientRects().length) {
                  closePin({ reason: 'gone', by: 'data', via: null });
                  return;
                }
                clipped = !list.at(-1).isIntersecting;
                page.pin.place();
              })
            : null;
        seen?.observe(n);
        const WATCHED = [...MARKS, 'data-hovercraft-mode', 'class', 'style', 'hidden'];
        const marks = new MutationObserver(() => page.pin?.handle === handle && page.pin.place());
        const watchLine = (a) => {
          marks.disconnect();
          for (let node = a; node && node.nodeType === 1; node = node.parentElement) {
            marks.observe(node, { attributes: true, attributeFilter: WATCHED });
          }
        };
        watchLine(n);
        page.pin = {
          handle,
          id: key,
          kind,
          text,
          owner,
          box: pbox,
          anchor: () => {
            if (!anchorEl?.isConnected) {
              const was = anchorEl;
              anchorEl = byId(key);
              if (seen && anchorEl !== was) {
                if (was) seen.unobserve(was);
                if (anchorEl) seen.observe(anchorEl);
              }
              if (anchorEl && anchorEl !== was) watchLine(anchorEl);
            }
            return anchorEl;
          },
          focusAt: () => {
            const a = page.pin?.anchor();
            return a ? focusOf(a) : null;
          },
          item: () => (anchorEl?.isConnected ? anchorEl : n),
          after: () => {
            const stop = page.pin?.focusAt();
            const perItem = typeof focusAt === 'function' || owner === doc.documentElement; // the standalone: the page
            return nextAfter(doc, perItem && stop ? stop : owner);
          },
          place: () => {
            const a = page.pin?.anchor();
            if (!a) return;
            copyMarks(pbox, anchorOf(a));
            place(pbox, anchorOf(a), s, x, pinBeside(a), atPointer(a) ? a : null, true, placementOf(a));
            if (clipped) pbox.style.visibility = 'hidden';
            room(pbox, pbox.dataset.side === 'sheet' && pbox.style.visibility !== 'hidden');
            if (pbox.style.visibility === 'hidden' && pbox.contains(doc.activeElement)) {
              focusOf(a).focus({ preventScroll: true });
              if (pbox.contains(doc.activeElement)) focusRoot(rootEl());
            }
          },
          check: (by) => {
            const a = page.pin?.anchor();
            if (!a?.getClientRects().length) closePin({ reason: 'gone', by, via: null });
            else {
              const fresh = contentFor(a);
              if (!fresh?.blocks.length) {
                closePin({ reason: 'gone', by: 'data', via: null });
                return;
              }
              if (cut(plainOf(fresh)) !== page.pin.text) {
                page.pin.text = cut(plainOf(fresh));
                fill(pbox, fresh, { pinned: true, onCopy });
                pbox.querySelector(`.${CSS}-close`)?.addEventListener('click', onClose);
                stopsOff(pbox);
                place(pbox, anchorOf(a), s, x, pinBeside(a), atPointer(a) ? a : null, true, placementOf(a));
                startLives(pbox);
              }
              page.pin.place();
            }
          },
          stop: () => {
            seen?.disconnect();
            marks.disconnect();
            offOutside();
            owner.removeEventListener('click', inside);
            pbox.removeEventListener('keydown', tipKeys);
            around?.removeEventListener('close', onAround);
            stack?.remove(entry);
          },
        };
        kit.pinMotion.set(true);
        stopsOff(pbox);
        page.pin.place();
        if (pbox.dataset.side === 'sheet') reveal(n, pbox);
        startLives(pbox);
        kit.sizes?.observe(pbox);
        listenPage(doc);
        emitTip('pinned', key, kind, null, text, { by, via: info.via ?? null }, from, n);
        say(doc, `${name}: details pinned. Tab to reach them.`, n);
        return true;
      }

      box.addEventListener('pointerover', over);
      box.addEventListener('pointerout', out);
      box.addEventListener('pointerdown', down);
      box.addEventListener('pointermove', moved);
      box.addEventListener('keydown', keys, true);
      box.addEventListener('keydown', escapeInPart);
      box.addEventListener('focusin', focus);
      box.addEventListener('focusout', blur);
      box.addEventListener('click', tap);
      kit.hint.addEventListener('pointerover', hintOver);
      kit.hint.addEventListener('pointerout', hintOut);

      const handle = {
        hideHint,
        closePin,
        pin: (id, info) => pinTo(id, info),
        close: (info = {}) => closePin({ reason: info.reason ?? 'host', by: info.by ?? 'host', via: info.via ?? null }),
        get pinned() {
          return pinned();
        },
        element: (id) => byId(id),
        box,
        pinElement: (n, info) => pinTo(recOf(n)?.id ?? '', info, n),
        itemAt,
        focusOf,
        showTap: (n, real = true) => {
          const c = contentFor(n);
          if (c) tapOn(n, real && lastPointer !== 'mouse');
        },
        under: (at) => at === doc.documentElement || at.contains(box) || box.contains(at),
        redescribe() {
          undescribe();
          describe();
          if (page.hint?.handle === handle) page.hint.place();
          if (page.pin?.handle === handle) page.pin.check('data');
        },
        refresh(info = {}) {
          describe();
          if (page.hint?.handle === handle) {
            if (page.hint.anchor.isConnected && page.hint.anchor.getClientRects().length) page.hint.place();
            else hideHint();
          }
          if (page.pin?.handle === handle) page.pin.check(info.by ?? 'data');
        },
        hide: () => hideHint(),
        destroy() {
          if (gone) return;
          gone = true;
          handles.delete(handle);
          hideHint();
          closePin({ reason: 'gone', by: 'data', via: null });
          watch.disconnect();
          undescribe();
          box.removeEventListener('pointerover', over);
          box.removeEventListener('pointerout', out);
          box.removeEventListener('pointerdown', down);
          box.removeEventListener('pointermove', moved);
          box.removeEventListener('keydown', keys, true);
          box.removeEventListener('keydown', escapeInPart);
          box.removeEventListener('focusin', focus);
          box.removeEventListener('focusout', blur);
          box.removeEventListener('click', tap);
          kit.hint.removeEventListener('pointerover', hintOver);
          kit.hint.removeEventListener('pointerout', hintOut);
        },
      };
      handles.add(handle);
      const outward = Object.freeze({
        pin: handle.pin,
        close: handle.close,
        get pinned() {
          return pinned();
        },
        element: handle.element,
        refresh: handle.refresh,
        hide: () => hideHint(),
        destroy: handle.destroy,
      });
      inner.set(outward, handle);
      return outward;
    }

    let tipCount = 0;
    const tipNumbers = new WeakMap();
    function numberOf(node) {
      if (!tipNumbers.has(node)) {
        tipCount += 1;
        tipNumbers.set(node, tipCount);
      }
      return tipNumbers.get(node);
    }

    function reasonTips(box, { selector, when = () => true, side = 'below' } = {}) {
      const t = tips({ root: box }, { box, selector, when, side });
      return Object.freeze({ hide: t.hide, destroy: t.destroy });
    }

    const ATTR = NAMES.attr;
    const KIT_ROOT = '[data-hovercraft-visual], .kit';
    const BSLIB = 'bslib-tooltip > *, bslib-popover > *, [data-bs-toggle="tooltip"], [data-bs-toggle="popover"]';
    const ROW = 'tr, li, [role="row"], [role="option"]';
    const ROWS = 'table, ul, ol, [role="table"], [role="grid"], [role="treegrid"], [role="list"], [role="listbox"]';
    const REFUSED =
      'a[href], button[type="submit"], button[type="reset"], input, select, textarea, summary, label, [contenteditable]:not([contenteditable="false"])';
    const KIND = /^[a-z][a-z0-9]*(_[a-z0-9]+)*$/;
    const KIND_DEFAULT = 'element';
    function define(name, blocks) {
      if (!tipText(name)) {
        note('define', 'define(name, blocks): the name must be text.');
        return false;
      }
      tooltip({ tooltip_templates: { ...tooltip().tooltip_templates, [name]: blocks } });
      return true;
    }
    const attached = new WeakMap();
    function attach(scope = document) {
      const at = scope?.nodeType === 9 ? scope.documentElement : scope;
      if (attached.has(at)) return attached.get(at);
      const PIN_ATTR = `${ATTR}-pin`;
      const PIN = `[${PIN_ATTR}]`;
      const REASON = `${ATTR}-reason`;
      const GIVEN = '[data-reason][aria-disabled="true"]';
      const selector = `[${ATTR}], [${ATTR}-template], [${REASON}], ${GIVEN}, ${PIN}`;
      const ours = (n) => !n.closest(KIT_ROOT) && !n.matches(BSLIB);
      const idOf = (n) => n.getAttribute(`${ATTR}-id`) || n.id || '';
      const kindOf = (n) => {
        const kind = n.getAttribute(`${ATTR}-kind`);
        if (kind === null) return KIND_DEFAULT;
        if (KIND.test(kind)) return kind;
        note(
          `kind|${kind}`,
          `${ATTR}-kind "${kind}" must be lower-case words joined with _; the events say "${KIND_DEFAULT}".`,
        );
        return KIND_DEFAULT;
      };
      const refusal = (n) => {
        const button = n.closest('button');
        if (n.closest(REFUSED) || (button?.form && (button.type === 'submit' || button.type === 'reset')))
          return 'its click does something else';
        if (!idOf(n)) return `it has no ${ATTR}-id or id`;
        return null;
      };
      const pinnable = (n) => !!n?.hasAttribute(PIN_ATTR) && n.matches(selector) && ours(n) && refusal(n) === null;
      const marked = new WeakSet();
      const t = tips(
        { root: at },
        {
          box: at,
          selector,
          pin: (n) => pinnable(n),
          beside: (n) => (n.matches(ROW) ? (n.parentElement?.closest(ROWS) ?? null) : null),
          align: (n) => (n.matches(ROW) ? 'pointer' : 'item'),
          record: (n) => {
            if (!ours(n)) return null;
            let row = {};
            const raw = n.getAttribute(`${ATTR}-record`);
            if (raw) {
              try {
                const parsed = JSON.parse(raw);
                if (tipObject(parsed)) row = parsed;
              } catch {
                note(`record|${raw}`, `${ATTR}-record must be JSON: ${raw}`);
              }
            }
            const text = n.getAttribute(ATTR);
            if (tipText(text)) row = { ...row, tooltip: text };
            const name = n.getAttribute(`${ATTR}-template`);
            if (name && !tipText(text)) own.set(n, { ...(own.get(n) ?? {}), tooltip_template: name });
            const why = n.getAttribute(REASON) ?? (n.matches(GIVEN) ? n.dataset.reason : null);
            const reason = tipText(why) ? why : null;
            if (reason && n.getAttribute('aria-disabled') !== 'true') {
              n.setAttribute('aria-disabled', 'true');
              marked.add(n);
            }
            if (n.hasAttribute(PIN_ATTR)) {
              const no = refusal(n);
              if (no)
                note(
                  `pin|${idOf(n)}|${no}`,
                  `${PIN_ATTR} is left out on ${idOf(n) || n.tagName}: ${no}; it shows as a hint.`,
                );
            }
            const label = labelOf(n);
            return { id: idOf(n), kind: kindOf(n), label, row, index: 0, reason, reason_prefix: true };
          },
        },
      );
      const h = inner.get(t);
      const pinAt = (target) => {
        const hit = target?.closest?.(PIN);
        if (hit) return hit;
        const item = h.itemAt(target);
        return item?.matches(PIN) ? item : null;
      };
      const flip = (n, via) => {
        if (t.pinned === idOf(n)) t.close({ reason: 'select', by: 'reader', via });
        else h.pinElement(n, { by: 'reader', via });
      };
      const onClick = (e) => {
        const n = pinAt(e.target);
        if (n && at.contains(n) && pinnable(n)) flip(n, e.detail === 0 ? 'key' : 'click');
      };
      const onKey = (e) => {
        if ((e.key !== 'Enter' && e.key !== ' ') || e.isComposing || e.defaultPrevented) return;
        if (e.target.matches?.('button')) return;
        const n = pinAt(e.target);
        if (!n || !at.contains(n) || !pinnable(n)) return;
        e.preventDefault();
        flip(n, 'key');
      };
      const blocked = (target) => {
        const n = target?.closest?.(`[${REASON}]`) ?? h.itemAt(target);
        return n && at.contains(n) && ours(n) && tipText(n.getAttribute(REASON)) ? n : null;
      };
      const stopClick = (e) => {
        const n = blocked(e.target);
        if (!n) return;
        if (t.pinned !== null && !pinnable(n))
          t.close({ reason: 'outside', by: 'reader', via: e.detail === 0 ? 'key' : 'click' });
        e.preventDefault();
        e.stopPropagation();
        h.showTap(n, e.detail !== 0);
      };
      const stopKey = (e) => {
        if ((e.key !== 'Enter' && e.key !== ' ') || e.isComposing || !blocked(e.target)) return;
        e.preventDefault();
        e.stopPropagation();
      };
      const reasons = new MutationObserver((records) => {
        for (const r of records) {
          const n = r.target;
          if (!tipText(n.getAttribute(REASON)) && marked.has(n)) {
            n.removeAttribute('aria-disabled');
            marked.delete(n);
          }
        }
        h.redescribe();
      });
      reasons.observe(at, {
        attributes: true,
        subtree: true,
        attributeFilter: [REASON, 'data-reason', 'aria-disabled'],
      });
      at.addEventListener('click', stopClick, true);
      at.addEventListener('keydown', stopKey, true);
      at.addEventListener('click', onClick);
      at.addEventListener('keydown', onKey);
      const handle = Object.freeze({
        destroy: () => {
          attached.delete(at);
          reasons.disconnect();
          at.removeEventListener('click', stopClick, true);
          at.removeEventListener('keydown', stopKey, true);
          at.removeEventListener('click', onClick);
          at.removeEventListener('keydown', onKey);
          t.destroy();
        },
      });
      attached.set(at, handle);
      return handle;
    }

    const block = (type, render) => {
      if (typeof type === 'string' && typeof render === 'function') renderers.set(type, render);
    };
    return Object.freeze({
      tips,
      reasonTips,
      tooltip,
      define,
      attach,
      block,
      contentOf,
      plainOf,
      settingsAt,
      placements: [...PLACEMENTS],
    });
  }

  const engine = createTipEngine({
    emit: (host, type, detail, item) => {
      const at = item?.isConnected ? item : host.root;
      at.dispatchEvent(new CustomEvent(type, { bubbles: true, detail }));
    },
    markdown,
    num,
    format: format_number,
    layer: () => null,
    motion: (el) => ({
      set: (open) => {
        el.hidden = !open;
      },
    }),
    outside: (root, fn) => {
      const doc = root.ownerDocument ?? document;
      doc.addEventListener('click', fn);
      return () => doc.removeEventListener('click', fn);
    },
  });
  window.Hovercraft = Object.freeze({
    version: VERSION,
    define: engine.define,
    attach: engine.attach,
    tooltip: engine.tooltip,
  });
})();
