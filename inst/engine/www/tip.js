// hovercraft tip engine 0.52.0, built from its source; do not edit.
(() => {
  const VERSION = '0.52.0';
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
  const MD_LINK_SCHEMES = ['http', 'https', 'mailto'];
  const MD_LINK_REL = 'noopener noreferrer';
  const MD_NOTE_EXCLUDED = ' \t\\`*[]^';
  const MD_NOTE_ATTRIBUTE = 'data-hc-note';
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

  function mdNoteEnd(s, i) {
    if (s[i + 1] !== '^') return -1;
    let j = i + 2;
    while (j < s.length && !MD_NOTE_EXCLUDED.includes(s[j])) j += 1;
    return j > i + 2 && s[j] === ']' ? j : -1;
  }

  function mdInline(s, links = true, notes = true) {
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
      } else if (c === '[' && links && notes && mdNoteEnd(s, i) !== -1) {
        const end = mdNoteEnd(s, i);
        flush();
        tokens.push(['note', s.slice(i + 2, end)]);
        i = end + 1;
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

  function mdScheme(href, words, at) {
    const found = /^([a-z][a-z0-9]*):/.exec(mdLower(href));
    if (!found || MD_LINK_SCHEMES.includes(found[1])) return null;
    if (!at.schemes || !Object.hasOwn(at.schemes, found[1]) || typeof at.schemes[found[1]] !== 'function') return null;
    try {
      const node = at.schemes[found[1]]({
        href,
        rest: href.slice(found[1].length + 1),
        words,
        field: at.field,
        id: at.id,
      });
      return node instanceof Node ? node : null;
    } catch {
      return null;
    }
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
      else if (p[0] === 'note') {
        const span = document.createElement('span');
        span.setAttribute(MD_NOTE_ATTRIBUTE, p[1]);
        span.textContent = `[^${p[1]}]`;
        add(span);
      } else if (p[3] && !at.words) {
        const a = mdLink(p[1], at);
        a.append(...mdNodes(p[2], at));
        add(a);
      } else {
        const words = mdNodes(p[2], at);
        const drawn = p[3] || at.words ? null : mdScheme(p[1], words, at);
        for (const node of drawn ? [drawn] : words) add(node);
      }
    }
    return out;
  }

  function markdown(el, text, { inline = false, field = null, id = null, notes = true, schemes = null } = {}) {
    const at = {
      field: field ?? null,
      id: id === null || id === undefined ? null : String(id),
      words: Boolean(el.closest?.(MD_PICKABLE)),
      schemes: schemes !== null && typeof schemes === 'object' ? schemes : null,
    };
    const read = notes !== false;
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
          for (const piece of mdInline(line, true, read)) pieces.push(piece);
        }
      }
      el.replaceChildren(...mdNodes(mdJoin(pieces), at));
      return el;
    }
    const nodes = parts.map((b) => {
      if (b[0] === 'p') {
        const p = document.createElement('p');
        p.append(...mdNodes(mdInline(b[1], true, read), at));
        return p;
      }
      const list = document.createElement(b[0]);
      if (b[0] === 'ol' && b[1] !== 1) list.setAttribute('start', String(b[1]));
      for (const item of b[2]) {
        const li = document.createElement('li');
        li.append(...mdNodes(mdInline(item, true, read), at));
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

  markdown.plain = (text, { notes = true } = {}) => {
    if (text === null || text === undefined) return '';
    const words = [];
    for (const b of mdBlocks(String(text))) {
      if (b[0] === 'p') words.push(mdPlain(mdInline(b[1], true, notes !== false)));
      else for (const item of b[2]) words.push(mdPlain(mdInline(item, true, notes !== false)));
    }
    return words.join(' ');
  };
  Object.freeze(markdown);
  const TIP_NAMES = Object.freeze({
    attr: 'data-hovercraft-tip',
    css: 'hc-tip',
    who: 'Hovercraft',
    standalone: true,
    skip: '',
  });

  function createTipEngine(env) {
    const NAMES = TIP_NAMES;
    const CSS = NAMES.css;
    const HINT_DELAY = 300;
    const PLACEMENTS = ['auto', 'top', 'bottom', 'left', 'right'];
    const PRESET_TEXT = {
      dense:
        '{"radius":2,"font":"ui-sans-serif, system-ui, -apple-system, \\"Segoe UI\\", sans-serif","size":0.95,"delay":350,"light":{"bg":"#FBFAF4","ink":"#141A20","line":"#1C2530","code_bg":"#ECEBE2","shadow":"0 1px 0 rgba(20, 26, 32, 0.08), 0 6px 14px -6px rgba(20, 26, 32, 0.45)"},"dark":{"bg":"#1A2129","ink":"#E8EDF2","line":"#6C7E90","code_bg":"#10151A","shadow":"0 1px 0 rgba(0, 0, 0, 0.4), 0 8px 16px -6px rgba(0, 0, 0, 0.8)"}}',
      editorial:
        '{"radius":2,"font":"\\"Iowan Old Style\\", \\"Palatino Linotype\\", Palatino, Georgia, serif","size":1.15,"delay":300,"light":{"bg":"#FFFCF4","ink":"#1C1A17","line":"#8A7F6C","code_bg":"#F1EAD8","shadow":"3px 3px 0 rgba(28, 26, 23, 0.14)"},"dark":{"bg":"#26221C","ink":"#ECE5D4","line":"#8A806B","code_bg":"#1F1C17","shadow":"3px 3px 0 rgba(0, 0, 0, 0.5)"}}',
      lively:
        '{"radius":12,"font":"ui-rounded, \\"Hiragino Maru Gothic ProN\\", Nunito, system-ui, sans-serif","size":1.05,"delay":300,"light":{"bg":"#FFFFFF","ink":"#1B2234","line":"#8493B8","shadow":"0 14px 30px -10px rgba(27, 34, 52, 0.18), 0 4px 10px -3px rgba(27, 34, 52, 0.09)"},"dark":{"bg":"#222940","ink":"#F4F6FC","line":"#6B7DB3","shadow":"0 18px 36px -8px rgba(5, 8, 18, 0.62), 0 4px 12px -2px rgba(5, 8, 18, 0.45)"}}',
      modern:
        '{"radius":8,"font":"ui-sans-serif, system-ui, -apple-system, \\"Segoe UI\\", Roboto, sans-serif","size":1,"delay":300,"light":{"bg":"#FFFFFF","ink":"#14171F","line":"#8A92A3","shadow":"0 1px 2px rgba(15, 20, 30, 0.05), 0 8px 24px -6px rgba(15, 20, 30, 0.1)"},"dark":{"bg":"#171A22","ink":"#EEF1F6","line":"#6F7A90","shadow":"0 2px 6px rgba(0, 0, 0, 0.35), 0 12px 28px -6px rgba(0, 0, 0, 0.55)"}}',
      sober:
        '{"radius":2,"font":"ui-sans-serif, system-ui, sans-serif","size":1,"delay":300,"light":{"bg":"#FBFAF6","ink":"#1C2128","line":"#59626D","shadow":"2px 2px 0 rgba(28, 33, 40, 0.16)"},"dark":{"bg":"#181C21","ink":"#E8E5DD","line":"#7F8994","shadow":"2px 2px 0 rgba(0, 0, 0, 0.55)"}}',
    };
    const PHONE = 600;
    const LEAVE_MS = 100;
    const GAP = 6;
    const ARROW = 5;
    const CUT = 200;
    const DEFAULT_LABELS = Object.freeze({
      copy: 'Copy',
      copied: 'Copied',
      copy_failed: 'Copy failed',
      close: 'Close',
      not_available: 'Not available:',
      pinned: '{label}: details pinned. Tab to reach them.',
      code_line: 'code: {first}',
      code_line_empty: 'code, 1 line',
      code_lines: 'code, {n} lines, starting {first}',
      code_lines_empty: 'code, {n} lines',
      more: 'More',
      more_pinned: 'More when pinned',
    });
    const formatLabel = (s, v) => String(s).replace(/\{(\w+)\}/g, (m, k) => (tipOwn(v, k) ? String(v[k]) : m));
    function labelsAt(from) {
      const out = { ...DEFAULT_LABELS };
      const stack = [];
      for (let n = from; n; n = n.parentElement) if (own.get(n)?.labels) stack.push(own.get(n).labels);
      while (stack.length) for (const [k, v] of stack.pop()) out[k] = v;
      return out;
    }
    const RESERVED = [
      'title',
      'text',
      'badge',
      'fields',
      'code',
      'group',
      'host',
      'html',
      'visual',
      'part',
      'widget',
      'rows',
      'steps',
      'spark',
      'actions',
      'compare',
      'need',
    ];
    const DASH = '–';
    const MODAL = 'dialog[open], [aria-modal="true"], .modal.show, .modal.in';
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
        return true;
      });
    };
    function usable(blocks, where) {
      const slots = new Set();
      return sound(
        blocks.filter((b) => {
          if (b.type === 'host') {
            const id = slotId(b.id);
            if (!id || slots.has(id)) {
              note(
                `host|${where}|${id}`,
                `${where}: a host block needs an "id" that no other host block has; it is left out.`,
              );
              return false;
            }
            slots.add(id);
          }
          if (registry.has(b.type)) return true;
          const later = RESERVED.includes(b.type) ? ' (it comes in a later version)' : '';
          note(
            `blk|${where}|${b.type}`,
            `${where}: no block "${b.type}" is registered${later}; it is left out until one is. Registered: ${[...registry.keys()].join(', ')}.`,
          );
          return false;
        }),
        where,
      );
    }
    const slotId = (v) => (tipText(v) || Number.isFinite(v) ? String(v) : '');
    const GROUP_DEPTH = 2;
    const hasParent = (b) => (typeof b.parent === 'string' && b.parent !== '') || Number.isFinite(b.parent);
    function sound(blocks, where) {
      if (!blocks.some((b) => b.type === 'group' || hasParent(b))) return blocks;
      const said = (id, why) => note(`grp|${where}|${id}|${why}`, `${where}: ${why}; it is left out.`);
      const groups = new Map();
      for (const b of blocks) {
        if (b.type !== 'group') continue;
        const id = tipText(b.id) || Number.isFinite(b.id) ? String(b.id) : '';
        if (!id) said('?', 'a group needs an "id" that its blocks name in "parent"');
        else if (groups.has(id)) said(id, `a second group has the id "${id}" (an id is used once)`);
        else groups.set(id, b);
      }
      const levels = new Map();
      const levelOf = (b, chain) => {
        if (levels.has(b)) return levels.get(b);
        let n = 1;
        if (hasParent(b)) {
          const up = groups.get(String(b.parent));
          const what = b.type === 'group' ? `the group "${b.id}"` : `a ${b.type} block`;
          if (!up) {
            said(`${b.type}|${b.id ?? ''}|${b.parent}`, `${what} names the parent "${b.parent}", which is no group`);
            n = 0;
          } else if (up === b || chain.includes(up)) {
            said(`loop|${b.id}`, `the groups "${b.id}" and "${b.parent}" hold each other`);
            n = 0;
          } else if (levelOf(up, [...chain, b]) === 0) {
            n = 0;
          } else n = levelOf(up, []) + 1;
        }
        if (b.type === 'group' && n > GROUP_DEPTH) {
          said(
            `deep|${b.id}`,
            `the group "${b.id}" is ${n} deep, and groups go ${GROUP_DEPTH} deep at most; give its blocks to the group above it, or move it to the top level`,
          );
          n = 0;
        }
        levels.set(b, n);
        return n;
      };
      const stands = (b) => (b.type === 'group' && groups.get(String(b.id)) !== b ? false : levelOf(b, []) > 0);
      let kept = blocks.filter(stands);
      for (let more = true; more; ) {
        more = false;
        const used = new Set(kept.filter(hasParent).map((b) => String(b.parent)));
        const next = kept.filter((b) => b.type !== 'group' || used.has(String(b.id)));
        for (const b of kept) if (!next.includes(b)) said(String(b.id), `the group "${b.id}" holds no block`);
        more = next.length !== kept.length;
        kept = next;
      }
      return kept;
    }
    function treeOf(blocks) {
      const nodes = blocks.map((b) => ({ b, kids: [] }));
      const groups = new Map();
      for (const n of nodes) if (n.b.type === 'group' && !groups.has(String(n.b.id))) groups.set(String(n.b.id), n);
      const top = [];
      for (const n of nodes) (hasParent(n.b) ? (groups.get(String(n.b.parent)) ?? { kids: top }).kids : top).push(n);
      return top;
    }
    function slotsOf(content) {
      const out = [];
      const walk = (nodes) => {
        for (const { b, kids } of nodes) {
          if (b.type === 'host') out.push(slotId(b.id));
          walk(kids);
        }
      };
      walk(treeOf(headFirst(content.blocks)));
      if (content.more) walk(treeOf(content.more.blocks));
      return out;
    }
    function placementWord(v, where) {
      const word = typeof v === 'string' ? v.trim() : '';
      if (PLACEMENTS.includes(word)) return word;
      note(
        `placement|${String(v)}`,
        `${where}: placement "${String(v)}" is not one of ${PLACEMENTS.join(', ')}; it reads as auto.`,
      );
      return 'auto';
    }
    const PRESET_NONE = 'none';
    const PRESET_ATTR = `${NAMES.attr}-preset`;
    const presetCache = new Map();
    const presetData = (name) => {
      if (!Object.hasOwn(PRESET_TEXT, name)) return null;
      if (!presetCache.has(name)) presetCache.set(name, JSON.parse(PRESET_TEXT[name]));
      return presetCache.get(name);
    };
    function presetWord(v, where) {
      const word = typeof v === 'string' ? v.trim() : '';
      if (word === PRESET_NONE || presetData(word)) return word;
      note(
        `preset|${String(v)}`,
        `${where}: preset "${String(v)}" is not one of ${Object.keys(PRESET_TEXT).join(', ')} (or ${PRESET_NONE}); it reads as ${PRESET_NONE}.`,
      );
      return PRESET_NONE;
    }
    function presetAt(from) {
      return presetFrom(from)?.name ?? null;
    }
    function presetFrom(from) {
      for (let node = from; node?.nodeType === 1; node = node.parentElement) {
        const attr = node.getAttribute(PRESET_ATTR);
        const mine = own.get(node);
        let word = null;
        if (attr !== null) word = presetWord(attr, PRESET_ATTR);
        else if (mine && tipOwn(mine, 'preset')) word = mine.preset;
        if (word) return word === PRESET_NONE ? null : { name: word, node };
      }
      return null;
    }
    const CLASS_ATTR = `${NAMES.attr}-class`;
    const CLASS_TOKEN = /^-?[_a-zA-Z][_a-zA-Z0-9-]*$/;
    const MORE_ON = ['pin', 'hover'];
    const MORE_CUE = ['icon', 'none'];
    const MORE_ON_ATTR = `${NAMES.attr}-more-on`;
    const MORE_CUE_ATTR = `${NAMES.attr}-more-cue`;
    function moreWord(v, words, where) {
      const word = typeof v === 'string' ? v.trim() : '';
      if (words.includes(word)) return word;
      note(
        `more|${where}|${String(v)}`,
        `${where}: "${String(v)}" is not ${words.join(' or ')}; it reads as ${words[0]}.`,
      );
      return words[0];
    }
    function classWord(v, where) {
      const out = [];
      for (const token of String(v).split(/\s+/).filter(Boolean)) {
        if (!CLASS_TOKEN.test(token))
          note(
            `class|${token}`,
            `${where}: "${token}" is not a class name (letters, digits, - and _, not starting with a digit); it is left out.`,
          );
        else if (token.startsWith(CSS))
          note(
            `class|${token}`,
            `${where}: "${token}" starts with "${CSS}", which the tip's own classes use; it is left out.`,
          );
        else if (!out.includes(token)) out.push(token);
      }
      return out;
    }
    function classesAt(from) {
      for (let node = from; node?.nodeType === 1; node = node.parentElement) {
        const attr = node.getAttribute(CLASS_ATTR);
        if (attr !== null) return classWord(attr, CLASS_ATTR);
        const mine = own.get(node);
        if (mine && tipOwn(mine, 'class')) return mine.class;
      }
      return [];
    }
    const classAdded = new WeakMap();
    function classed(box, anchor) {
      const want = classesAt(anchor);
      const had = classAdded.get(box) ?? [];
      for (const token of had) if (!want.includes(token)) box.classList.remove(token);
      const added = [];
      for (const token of want) {
        if (had.includes(token) || !box.classList.contains(token)) {
          box.classList.add(token);
          added.push(token);
        }
      }
      if (added.length) classAdded.set(box, added);
      else classAdded.delete(box);
    }
    function unclass(box) {
      for (const token of classAdded.get(box) ?? []) box.classList.remove(token);
      classAdded.delete(box);
    }
    const OPEN_ATTR = `${NAMES.attr}-open`;
    const PART = `[${NAMES.attr}-part]`;
    function markOpen(el, kind) {
      if (el?.nodeType === 1 && el.getAttribute(OPEN_ATTR) !== kind) el.setAttribute(OPEN_ATTR, kind);
    }
    function unmarkOpen(el, kind) {
      if (el?.nodeType === 1 && el.getAttribute(OPEN_ATTR) === kind) el.removeAttribute(OPEN_ATTR);
    }
    const ANCHORS = ['item', 'content'];
    const ANCHOR_ATTR = `${NAMES.attr}-anchor`;
    function anchorWord(v, where) {
      const word = typeof v === 'string' ? v.trim() : '';
      if (ANCHORS.includes(word)) return word;
      if (typeof v !== 'string' || word) {
        note(
          `anchor|${where}|${String(v)}`,
          `${where}: "${String(v)}" is not ${ANCHORS.join(' or ')}; it reads as ${ANCHORS[0]}.`,
        );
      }
      return ANCHORS[0];
    }
    function anchorAt(from) {
      for (let node = from; node?.nodeType === 1; node = node.parentElement) {
        const attr = node.getAttribute(ANCHOR_ATTR);
        if (attr !== null) return anchorWord(attr, ANCHOR_ATTR);
        const mine = own.get(node);
        if (mine && tipOwn(mine, 'anchor')) return mine.anchor;
      }
      return ANCHORS[0];
    }
    const REPLACED = 'img, svg, canvas, video, iframe, input, select, textarea, button, meter, progress, object, embed';
    function contentBox(el) {
      const doc = el.ownerDocument;
      const view = doc.defaultView;
      if (!view) return null;
      const rects = [];
      const take = (parent) => {
        for (const c of parent.childNodes) {
          if (c.nodeType === 3) {
            if (!c.nodeValue.trim()) continue;
            const range = doc.createRange();
            range.selectNodeContents(c);
            rects.push(...range.getClientRects());
          } else if (c.nodeType === 1 && !c.hidden && !c.classList.contains(`${CSS}-desc`)) {
            const style = view.getComputedStyle(c);
            if (style.display === 'none' || style.visibility === 'hidden') continue;
            if (style.display === 'contents' || (!style.display.startsWith('inline') && !c.matches(REPLACED))) take(c);
            else rects.push(...c.getClientRects());
          }
        }
      };
      take(el);
      const seen = rects.filter((r) => r.width > 0 && r.height > 0).sort((a, b) => a.top - b.top);
      if (!seen.length) return null;
      let left = Number.POSITIVE_INFINITY;
      let right = Number.NEGATIVE_INFINITY;
      let top = seen[0].top;
      let bottom = seen[0].bottom;
      for (const r of seen) {
        const mid = (r.top + r.bottom) / 2;
        if (mid > bottom + 1 || mid < top - 1) return null;
        left = Math.min(left, r.left);
        right = Math.max(right, r.right);
        top = Math.min(top, r.top);
        bottom = Math.max(bottom, r.bottom);
      }
      return { left, right, top, bottom, width: right - left, height: bottom - top };
    }
    const aimAt = (item, anchor, line) => {
      const at = item && anchor.contains(item) ? item : anchor;
      return !line && anchorAt(at) === ANCHORS[1] ? contentBox(at) : null;
    };
    const CODE_FILL = 'color-mix(in srgb, var(--hc-tip-ink) 12%, transparent)';
    const dressed = new WeakMap();
    function dress(box, anchor, mode) {
      const at = presetFrom(anchor);
      const preset = at ? presetData(at.name) : null;
      const set = preset ? presetValues(preset, mode) : {};
      const root = anchor.ownerDocument.documentElement;
      const above = preset ? (at.node === root ? root : at.node.parentElement) : box.parentElement;
      for (const [prop, value] of declaredAt(anchor, above, CARRIED)) set[prop] = value;
      for (const [prop, value] of declaredAt(anchor, box.parentElement, ['layer'])) set[prop] = value;
      const made = Object.entries(set)
        .filter(([, value]) => value !== undefined && value !== null)
        .map(([prop, value]) => [prop.startsWith('--') ? prop : `--hovercraft-tip-${prop}`, String(value)]);
      const name = preset ? at.name : null;
      if (box.getAttribute(PRESET_ATTR) !== name) {
        if (name) box.setAttribute(PRESET_ATTR, name);
        else box.removeAttribute(PRESET_ATTR);
      }
      const now = new Set(made.map(([prop]) => prop));
      for (const prop of dressed.get(box) ?? []) if (!now.has(prop)) box.style.removeProperty(prop);
      for (const [prop, value] of made)
        if (box.style.getPropertyValue(prop) !== value) box.style.setProperty(prop, value);
      if (now.size) dressed.set(box, [...now]);
      else dressed.delete(box);
    }
    function presetValues(p, mode) {
      const s = p[mode] ?? p.light;
      return {
        radius: `${p.radius}px`,
        font: p.font,
        size: p.size,
        bg: s.bg,
        ink: s.ink,
        line: s.line,
        'code-bg': s.code_bg ?? CODE_FILL,
        shadow: s.shadow ?? p.light.shadow ?? 'var(--hovercraft-shadow-2)',
        gap: `${p.gap ?? GAP}px`,
        'max-width': `${p.max_width ?? 320}px`,
        'pin-width': `${p.pin_width ?? 420}px`,
        motion: p.motion ?? 'initial',
      };
    }
    function delayAt(anchor) {
      const name = presetAt(anchor);
      if (name) return presetData(name).delay ?? HINT_DELAY;
      const ms = Number.parseFloat(cssProp(anchor, '--hovercraft-tip-delay'));
      return Number.isFinite(ms) && ms >= 0 && ms <= 800 ? ms : HINT_DELAY;
    }
    function tooltip(settings, el) {
      const at = el ?? document.documentElement;
      if (settings !== undefined && settings !== null) {
        if (!tipObject(settings))
          note(
            'settings',
            'tooltip: give { tooltip, tooltip_template, tooltip_templates, tooltip_placement, tooltip_arrow, tooltip_class, tooltip_anchor, tooltip_preset, tooltip_labels, tooltip_more_template, tooltip_more_on, tooltip_more_cue, tooltip_more_height } (or nothing to read the setting).',
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
          if (tipOwn(settings, 'tooltip_arrow')) {
            const v = settings.tooltip_arrow;
            if (v === null || v === undefined) delete mine.arrow;
            else if (typeof v === 'boolean') mine.arrow = v;
            else note(`arrow|${String(v)}`, 'tooltip: "tooltip_arrow" must be true or false; kept the old value.');
          }
          if (tipOwn(settings, 'tooltip_class')) {
            const v = settings.tooltip_class;
            if (v === null || v === undefined) delete mine.class;
            else if (typeof v === 'string') mine.class = classWord(v, 'tooltip');
            else
              note(`classes|${String(v)}`, 'tooltip: "tooltip_class" must be class names as text; kept the old value.');
          }
          if (tipOwn(settings, 'tooltip_anchor')) {
            const v = settings.tooltip_anchor;
            if (v === null || v === undefined) delete mine.anchor;
            else mine.anchor = anchorWord(v, 'tooltip_anchor');
          }
          if (tipOwn(settings, 'tooltip_preset')) {
            const v = settings.tooltip_preset;
            if (v === null || v === undefined) delete mine.preset;
            else mine.preset = presetWord(v, 'tooltip');
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
          if (tipOwn(settings, 'tooltip_labels')) {
            const v = settings.tooltip_labels;
            if (v === null || v === undefined) delete mine.labels;
            else if (Array.isArray(v) && !v.length) mine.labels = new Map();
            else if (!tipObject(v)) note('labels', 'tooltip: "tooltip_labels" must be { key: text }.');
            else {
              mine.labels = new Map();
              for (const [k, t] of Object.entries(v)) {
                if (!tipOwn(DEFAULT_LABELS, k)) note(`label-key|${k}`, `tooltip: "${k}" is not a label; left out.`);
                else if (t !== null && t !== undefined)
                  tipText(t)
                    ? mine.labels.set(k, t)
                    : note(`label-val|${k}|${String(t)}`, `tooltip: label "${k}" must be text; left out.`);
              }
            }
          }
          if (tipOwn(settings, 'tooltip_more_template')) {
            const v = settings.tooltip_more_template;
            if (v === null || v === undefined) delete mine.tooltip_more_template;
            else if (tipText(v)) mine.tooltip_more_template = v;
            else
              note(
                `more-template|${String(v)}`,
                'tooltip: "tooltip_more_template" must be a template\'s name; kept the old value.',
              );
          }
          for (const [key, words, own_key] of [
            ['tooltip_more_on', MORE_ON, 'more_on'],
            ['tooltip_more_cue', MORE_CUE, 'more_cue'],
          ]) {
            if (!tipOwn(settings, key)) continue;
            const v = settings[key];
            if (v === null || v === undefined) delete mine[own_key];
            else mine[own_key] = moreWord(v, words, 'tooltip');
          }
          if (tipOwn(settings, 'tooltip_more_height')) {
            const v = settings.tooltip_more_height;
            if (v === null || v === undefined) delete mine.more_height;
            else if (lengthOf(v)) mine.more_height = lengthOf(v);
            else
              note(
                `more-height|${String(v)}`,
                'tooltip: "tooltip_more_height" must be a length such as 12rem, 40vh or 240 (pixels); kept the old value.',
              );
          }
          own.set(at, mine);
          for (const h of handles) if (h.under(at)) h.redescribe();
        }
      }
      const got = settingsAt(at);
      const more = got.more();
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
        tooltip_arrow: got.arrow,
        tooltip_class: classesAt(at).join(' ') || null,
        tooltip_anchor: anchorAt(at),
        tooltip_preset: presetAt(at),
        tooltip_labels: got.labels,
        tooltip_more_template: got.more_template,
        tooltip_more_on: more.on,
        tooltip_more_cue: more.cue,
        tooltip_more_height: more.height,
      };
    }
    function settingsAt(el) {
      let on = null;
      let name = null;
      let namedAt = null;
      let placed = null;
      let arrow = null;
      let longName = null;
      let longAt = null;
      for (
        let node = el;
        node && (on === null || name === null || placed === null || arrow === null || longName === null);
        node = node.parentElement
      ) {
        const mine = own.get(node);
        if (!mine) continue;
        if (placed === null && tipOwn(mine, 'placement')) placed = mine.placement;
        if (arrow === null && tipOwn(mine, 'arrow')) arrow = mine.arrow;
        if (on === null && tipOwn(mine, 'tooltip')) on = mine.tooltip;
        if (name === null && tipOwn(mine, 'tooltip_template')) {
          name = mine.tooltip_template;
          namedAt = node;
        }
        if (longName === null && tipOwn(mine, 'tooltip_more_template')) {
          longName = mine.tooltip_more_template;
          longAt = node;
        }
      }
      const template = (wanted) => {
        for (let node = el; node; node = node.parentElement) {
          const found = own.get(node)?.templates?.get(wanted);
          if (found) return found;
        }
        return null;
      };
      const more = () => {
        const out = { on: MORE_ON[0], cue: MORE_CUE[0], height: null };
        const seen = new Set();
        for (let node = el; node?.nodeType === 1 && seen.size < 3; node = node.parentElement) {
          const mine = own.get(node);
          for (const [key, attr, words, setting] of [
            ['on', MORE_ON_ATTR, MORE_ON, 'more_on'],
            ['cue', MORE_CUE_ATTR, MORE_CUE, 'more_cue'],
          ]) {
            if (seen.has(key)) continue;
            const given = node.getAttribute(attr);
            if (given !== null) out[key] = moreWord(given, words, attr);
            else if (mine && tipOwn(mine, setting)) out[key] = mine[setting];
            else continue;
            seen.add(key);
          }
          if (!seen.has('height') && mine && tipOwn(mine, 'more_height')) {
            out.height = mine.more_height;
            seen.add('height');
          }
        }
        return out;
      };
      return {
        tooltip: on !== false,
        tooltip_template: name,
        named_at: namedAt,
        more_template: longName,
        more_named_at: longAt,
        more,
        template,
        placement: placed ?? 'auto',
        arrow: arrow !== false,
        labels: labelsAt(el),
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
      let longForm = null;
      const templated = rec.row !== null;
      if (at.tooltip && Array.isArray(over) && over.length) blocks = usable(over, "a part's tooltip");
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
            `the tooltip of ${String(rec.id)} must be text (a list of blocks comes in a later version); it has none.`,
          );
        }
        const named = rec.own_template_only && !at.own_template ? null : at.tooltip_template;
        if (!blocks && named) {
          const found = at.template(named);
          if (found) blocks = usable(found, `tooltip template "${named}"`);
          else note(`no-template|${named}`, `tooltip template "${named}" is not defined; no tooltip is drawn from it.`);
        }
        longForm = moreOf(rec, row, at, blocks);
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
        labels: at?.labels ?? DEFAULT_LABELS,
        more: longForm,
      };
    }
    function moreOf(rec, row, at, blocks) {
      let long = null;
      let literal = false;
      const t = row.tooltip_more;
      if (tipText(t) || (typeof t === 'number' && Number.isFinite(t))) {
        long = [{ type: 'text', text: typeof t === 'number' ? String(env.num(t)) : t }];
        literal = true;
      } else if (t !== null && t !== undefined && t !== '' && typeof t !== 'string') {
        note(`row-more|${rec.id}`, `the long form of ${String(rec.id)} must be text; it has none.`);
      } else if (!(rec.own_template_only && !at.own_more_template) && at.more_template) {
        const found = at.template(at.more_template);
        if (found) long = usable(found, `tooltip more template "${at.more_template}"`);
        else
          note(
            `no-more-template|${at.more_template}`,
            `more template "${at.more_template}" is not defined; no long form.`,
          );
      }
      if (!long?.length) return null;
      if (!blocks?.length) {
        note(
          `more-alone|${literal ? rec.id : at.more_template}`,
          `${literal ? rec.id : at.more_template} has a long form but no short form; a long form alone draws nothing.`,
        );
        return null;
      }
      const taken = new Set(blocks.filter((b) => b.type === 'host').map((b) => slotId(b.id)));
      const kept = long.filter((b) => {
        if (b.type !== 'host' || !taken.has(slotId(b.id))) return true;
        note(
          `more-host|${slotId(b.id)}`,
          `host block "${slotId(b.id)}" is in the short form already; left out of the long form.`,
        );
        return false;
      });
      if (!kept.length) return null;
      const m = at.more();
      return { blocks: kept, literal, on: m.on, cue: m.cue, height: m.height, canPin: false };
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
    const wordsOf = (content, text, literal = content.literal) =>
      literal ? String(text ?? '') : asWords(expand(text ?? '', content.record));
    const markdownOf = (content, text, literal = content.literal) =>
      literal ? String(text ?? '') : asMarkdown(expand(text ?? '', content.record));

    function codeOf(record, block) {
      if (!tipText(block.field)) return null;
      if (!tipOwn(record, block.field)) {
        note(
          `code|${block.field}`,
          `the tooltip code field "${block.field}" is not in the record; the block is left out.`,
        );
        return null;
      }
      const v = record[block.field];
      return typeof v === 'string' && v !== '' ? v : null;
    }

    const headFirst = (blocks) => {
      const i = blocks.findIndex((b) => b.type === 'title' && !hasParent(b));
      return i > 0 ? [blocks[i], ...blocks.slice(0, i), ...blocks.slice(i + 1)] : blocks;
    };
    function plainOf(content, withReason = true, form = 'pin') {
      const out = [content.label ?? ''];
      const read = (nodes, ctx) => {
        for (const { b, kids } of nodes) {
          const def = registry.get(b.type);
          if (!def) continue;
          try {
            const said = def.flatten(b, content.record, ctx);
            if (typeof said === 'string') out.push(said);
          } catch (e) {
            note(`flatten|${b.type}`, `a tooltip ${b.type} block could not be read out: ${e?.message ?? e}`);
          }
          read(kids, ctx);
        }
      };
      read(treeOf(headFirst(content.blocks)), flatCtx(content));
      if (content.more && (form === 'pin' || (form === 'hint' && content.more.on === 'hover')))
        read(treeOf(content.more.blocks), flatCtx(content, content.more.literal));
      if (withReason && content.reason) out.push(reasonWords(content));
      return out.filter((w) => w !== '').join(' ');
    }
    const stable = (v) => {
      if (Array.isArray(v)) return `[${v.map(stable).join(',')}]`;
      if (v !== null && typeof v === 'object')
        return `{${Object.keys(v)
          .sort()
          .map((k) => `${JSON.stringify(k)}:${stable(v[k])}`)
          .join(',')}}`;
      return JSON.stringify(v) ?? 'null';
    };
    const keyOf = (c) => {
      const { tip_index: _place, ...record } = c.record;
      return stable({
        name: String(c.rec.label ?? ''),
        label: c.label,
        reason: c.reason,
        prefix: c.prefix,
        literal: c.literal,
        labels: c.labels,
        record,
        blocks: c.blocks,
        more: c.more && { ...c.more },
        words: plainOf(c),
      });
    };
    const flatCtx = (content, literal = content.literal) => ({
      record: content.record,
      cls: CSS,
      labels: content.labels,
      words: (text) => wordsOf(content, text, literal),
      plain: (text) => env.markdown.plain(markdownOf(content, text, literal), { notes: false }),
    });
    const reasonWords = (content) =>
      !content.reason
        ? ''
        : content.blocks.length || content.prefix
          ? `${content.labels.not_available} ${content.reason}`
          : content.reason;
    const titleOf = (content) => {
      const t = content.blocks.find((b) => b.type === 'title' && !hasParent(b));
      return t ? wordsOf(content, t.text) : '';
    };
    const hostInfo = () => ({ reason: 'host', by: 'host', via: null });
    const goneInfo = () => ({ reason: 'gone', by: 'data', via: null });
    const cut = (text) => (text.length > CUT ? `${text.slice(0, CUT)}…` : text);
    const HINT_HOLDS = ['leave', 'blur', 'outside'];
    const HINT_VIA = ['pointer', 'focus', 'tap', 'key'];
    const hintWhy = (why, pointer) => {
      const by = why.by ?? 'reader';
      let via = why.via ?? null;
      if (by === 'data') via = null;
      else if (by === 'host') via = 'command';
      else if (via === 'click') via = pointer === 'mouse' ? 'pointer' : 'tap';
      else if (!HINT_VIA.includes(via)) via = 'pointer';
      return { reason: why.reason, by, via };
    };

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
      hint.setAttribute('aria-hidden', 'true');
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
          ? new view.ResizeObserver((entries) => {
              const page = pageOf(doc);
              page.hint?.place();
              page.pin?.place();
              for (const { target } of entries) resizeLives(target);
            })
          : null;
      const made = {
        hint,
        pin,
        live,
        sizes,
        hintMotion: tipMotion(hint),
        pinMotion: tipMotion(pin),
      };
      boxes.set(doc, made);
      return made;
    }
    function modeOf(anchor) {
      const read = (attr) => anchor.closest(`[${attr}]`)?.getAttribute(attr);
      const page = anchor.ownerDocument;
      const root = (attr) => page.documentElement.getAttribute(attr) ?? page.body?.getAttribute(attr);
      let mode = read('data-hovercraft-mode') ?? (NAMES.standalone ? root('data-bs-theme') : 'light');
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
      if (host !== doc.body && typeof box.showPopover === 'function') box.setAttribute('popover', 'manual');
      else if (box.hasAttribute('popover')) {
        drop(box);
        box.removeAttribute('popover');
      }
      copyMarks(box, anchor, false);
    }
    const ESCAPE_OWN =
      'input:not([type="checkbox"]):not([type="radio"]):not([type="button"]):not([type="submit"]):not([type="reset"]), textarea, select, [role="combobox"], [contenteditable]:not([contenteditable="false"])';
    function inModal(box, open, close, onKey) {
      const doc = box.ownerDocument;
      const host = box.parentElement;
      const View = doc.defaultView;
      if (!host || host === doc.body || host.localName === 'dialog' || !host.matches(MODAL)) return () => {};
      const keys = (e) => {
        if (e.key !== 'Escape' || e.isComposing || e.defaultPrevented || !open()) return;
        const at = doc.activeElement;
        if (box.contains(at) || at?.closest?.(ESCAPE_OWN)) return;
        onKey(e);
      };
      host.addEventListener('keydown', keys, true);
      const gone = () => {
        if (!open()) return;
        if (!host.isConnected || !host.matches(MODAL)) return close();
        const style = View?.getComputedStyle(host);
        if (style?.display === 'none' || style?.visibility === 'hidden') close();
      };
      const watch = typeof View?.MutationObserver === 'function' ? new View.MutationObserver(gone) : null;
      watch?.observe(host, { attributes: true, attributeFilter: ['class', 'style', 'aria-modal', 'hidden'] });
      watch?.observe(doc.body ?? doc.documentElement, { childList: true, subtree: true });
      return () => {
        host.removeEventListener('keydown', keys, true);
        watch?.disconnect();
      };
    }
    const tipMotion = (b) =>
      lifted(
        b,
        env.motion(b, {
          axis: () => motionOf(b),
          side: () => MOTION_SIDES[b.dataset.side] ?? 'top',
          distance: 4,
          rise: 0,
          exit: 'none',
        }),
      );
    function lifted(box, shows) {
      return {
        set: (open) => {
          if (open) {
            (shows.quiet ?? shows.set)(true);
            lift(box);
          } else {
            drop(box);
            shows.set(false);
          }
        },
        enter: () => shows.enter?.(),
      };
    }
    const isLifted = (box) => typeof box.showPopover === 'function' && box.matches(':popover-open');
    function lift(box) {
      if (!box.hasAttribute('popover') || !box.isConnected || isLifted(box)) return;
      try {
        box.showPopover();
      } catch {
        box.removeAttribute('popover');
      }
    }
    function drop(box) {
      if (isLifted(box)) box.hidePopover();
    }
    const cssProp = (el, k) => el.ownerDocument.defaultView?.getComputedStyle(el).getPropertyValue(k).trim() ?? '';
    const MOTION_SIDES = { above: 'bottom', below: 'top', left: 'right', right: 'left', sheet: 'bottom' };
    function motionOf(box) {
      const w = cssProp(box, '--hovercraft-tip-motion');
      return w === 'fade' || w === 'slide' || w === 'none' ? w : NAMES.standalone ? 'none' : 'fade';
    }
    function gapOf(box) {
      const raw = cssProp(box, '--hovercraft-tip-gap');
      const m = /^(-?(?:\d+\.?\d*|\.\d+))(px)?$/i.exec(raw);
      return m ? Math.min(24, Math.max(0, Number(m[1]))) : GAP;
    }
    const CARRIED =
      'bg ink line radius shadow code-bg size font gap max-width pin-width motion cue more-max-height'.split(' ');
    function declaredAt(anchor, above, keys) {
      const found = new Map();
      const view = anchor.ownerDocument.defaultView;
      if ((above && !above.contains(anchor)) || typeof view?.getComputedStyle !== 'function') return found;
      const at = view.getComputedStyle(anchor);
      const over = above ? view.getComputedStyle(above) : null;
      for (const key of keys) {
        const prop = `--hovercraft-tip-${key}`;
        const value = at.getPropertyValue(prop).trim();
        const outer = over?.getPropertyValue(prop).trim() ?? '';
        if (value !== outer) found.set(prop, value || 'initial');
      }
      return found;
    }
    function copyMarks(box, anchor, flip = true) {
      for (const mark of MARKS) {
        const from = anchor.closest(`[${mark}]`);
        if (from) box.setAttribute(mark, from.getAttribute(mark));
        else box.removeAttribute(mark);
      }
      const next = modeOf(anchor);
      if (!NAMES.standalone) box.setAttribute('data-hovercraft-mode', next);
      dress(box, anchor, next);
      classed(box, anchor);
      const was = modes.get(box);
      modes.set(box, next);
      anchors.set(box, anchor);
      if (flip && was && was !== next) flipLives(box, next);
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
    function place(
      box,
      anchor,
      side,
      x = null,
      beside = null,
      line = null,
      isPin = false,
      placement = 'auto',
      arrow = false,
      item = null,
    ) {
      const baseGap = gapOf(box);
      const reach = baseGap === GAP ? '' : `${baseGap}px`;
      if (box.style.getPropertyValue('--hc-tip-reach') !== reach) {
        if (reach) box.style.setProperty('--hc-tip-reach', reach);
        else box.style.removeProperty('--hc-tip-reach');
      }
      const to = placeBox(box, anchor, side, x, beside, line, isPin, placement, arrow ? ARROW : 0, baseGap, item);
      if (point(box, arrow ? to : null)) return;
      if (arrow) placeBox(box, anchor, side, x, beside, line, isPin, placement, 0, baseGap, item);
    }
    const EDGE = { below: 'top', above: 'bottom', right: 'left', left: 'right' };
    const AT = '--hc-tip-arrow-at';
    const BASE = 12;
    const solid = (colour) => {
      const alpha = /^rgba\(.*,\s*([\d.]+)\)$/.exec(colour) ?? /\/\s*([\d.]+)(%?)\)$/.exec(colour);
      return alpha ? Number.parseFloat(alpha[1]) / (alpha[2] ? 100 : 1) >= 1 : colour !== 'transparent';
    };
    function point(box, to) {
      if (box.style.visibility === 'hidden') return !!box.dataset.arrow;
      const edge = to && EDGE[box.dataset.side];
      const view = box.ownerDocument.defaultView;
      const style = view.getComputedStyle(box);
      if (edge && box.dataset.arrow !== edge) box.dataset.arrow = edge;
      const across = edge === 'top' || edge === 'bottom';
      const corner = Number.parseFloat(style.borderTopLeftRadius) || 0;
      const len = across ? box.offsetWidth : box.offsetHeight;
      const left = Number.parseFloat(box.style.left) || 0;
      const top = Number.parseFloat(box.style.top) || 0;
      const gap = !edge
        ? 0
        : {
            top: top - to.b,
            bottom: to.t - (top + box.offsetHeight),
            left: left - to.r,
            right: to.l - (left + box.offsetWidth),
          }[edge];
      const bridge = Math.max(0, -Number.parseFloat(view.getComputedStyle(box, '::before').right) || 0);
      const tall = box.offsetHeight >= Number.parseFloat(style.maxHeight) - 1;
      const none =
        !edge ||
        len - corner * 2 < BASE ||
        gap < ARROW - 0.5 ||
        !solid(style.backgroundColor) ||
        tall ||
        box.scrollWidth > box.clientWidth + bridge + 1;
      if (none) {
        if (box.hasAttribute('data-arrow')) box.removeAttribute('data-arrow');
        if (box.style.getPropertyValue(AT)) box.style.removeProperty(AT);
        return false;
      }
      const line = box.clientTop;
      const near = corner + BASE / 2;
      const from = across ? left : top;
      const [lo, hi] = across ? [to.l, to.r] : [to.t, to.b];
      const [from0, from1] = [Math.max(lo, from), Math.min(hi, from + len)];
      const where = (from0 <= from1 ? (from0 + from1) / 2 : (lo + hi) / 2) - from;
      const along = Math.max(near, Math.min(len - near, where)) - line - 4;
      const at = `${along}px`;
      if (box.style.getPropertyValue(AT) !== at) box.style.setProperty(AT, at);
      return true;
    }
    function placeBox(box, anchor, side, x, beside, line, isPin, placement, grow, baseGap = GAP, item = null) {
      const g = baseGap + grow;
      const doc = anchor.ownerDocument;
      const whole = anchor.getBoundingClientRect();
      const r = aimAt(item, anchor, line) ?? whole;
      const vw = doc.documentElement.clientWidth;
      const vh = doc.documentElement.clientHeight;
      const probe = line && x !== null ? line : anchor;
      const pr = probe === anchor ? whole : probe.getBoundingClientRect();
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
      if (placement !== 'auto' && vw >= PHONE) {
        const hit = placeOn(box, anchor, placement, x, line, isPin, grow, baseGap, r);
        if (hit) return hit;
      }
      const w = box.offsetWidth;
      const h = box.offsetHeight;
      const part = beside?.getBoundingClientRect();
      const rtl = part && doc.defaultView?.getComputedStyle(beside).direction === 'rtl';
      const fitsRight = part && part.right + g + w <= vw - 4;
      const fitsLeft = part && part.left - g - w >= 4;
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
        const row = line ? line.getBoundingClientRect() : r;
        const edge = part ?? row;
        let left = rtl ? edge.left : edge.right - w;
        let top = row.bottom + g;
        box.dataset.side = 'below';
        if (top + h > vh - 4) {
          top = row.top - h - g;
          box.dataset.side = 'above';
        }
        left = Math.max(4, Math.min(left, vw - w - 4));
        top = Math.max(4, Math.min(top, vh - h - 4));
        box.style.left = `${Math.round(left)}px`;
        box.style.top = `${Math.round(top)}px`;
        return { l: row.left, r: row.right, t: row.top, b: row.bottom };
      }
      if (line && !isPin && !room) {
        const row = line.getBoundingClientRect();
        const rtl = doc.defaultView?.getComputedStyle(line).direction === 'rtl';
        const gap = x === null ? g : baseGap + 8 + grow;
        const from = x === null ? (rtl ? r.left : r.right) : x;
        const ends = rtl ? from - gap - w : from + gap;
        const starts = rtl ? from + gap : from - gap - w;
        const fits = (at) => at >= 4 && at + w <= vw - 4;
        const at = fits(ends) ? ends : fits(starts) ? starts : null;
        if (at !== null) {
          box.dataset.side = at >= from ? 'right' : 'left';
          box.style.left = `${Math.round(at)}px`;
          box.style.top = `${Math.round(Math.max(4, Math.min(row.top, vh - h - 4)))}px`;
          return { l: from, r: from, t: row.top, b: row.bottom };
        }
      }
      const aside = side === 'right' || side === 'left';
      let left =
        side === 'right'
          ? r.right + g + 2
          : side === 'left'
            ? r.left - g - 2 - w
            : room === 'right'
              ? part.right + g
              : room === 'left'
                ? part.left - g - w
                : x === null
                  ? doc.defaultView?.getComputedStyle(anchor).direction === 'rtl'
                    ? r.right - w
                    : r.left
                  : x - w / 2;
      let top = aside ? r.top + (r.height - h) / 2 : room ? r.top : r.bottom + g;
      box.dataset.side = aside ? side : (room ?? 'below');
      if (!aside && !room && top + h > vh - 4) {
        top = r.top - h - g;
        box.dataset.side = 'above';
      }
      left = Math.max(4, Math.min(left, vw - w - 4));
      top = Math.max(4, Math.min(top, vh - h - 4));
      box.style.left = `${Math.round(left)}px`;
      box.style.top = `${Math.round(top)}px`;
      return { l: x ?? r.left, r: x ?? r.right, t: r.top, b: r.bottom };
    }

    function placeOn(box, anchor, want, x, line, isPin, grow, baseGap = GAP, r = anchor.getBoundingClientRect()) {
      const doc = anchor.ownerDocument;
      const vw = doc.documentElement.clientWidth;
      const vh = doc.documentElement.clientHeight;
      const w = box.offsetWidth;
      const h = box.offsetHeight;
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
      const g = baseGap + grow;
      const gapX = at === null ? g + 2 : baseGap + 8 + grow;
      const midX = (t.left + t.right) / 2 - w / 2;
      const midY = (t.top + t.bottom) / 2 - h / 2;
      const spots = {
        top: { left: midX, top: t.top - g - h, fits: t.top - g - h >= 4, side: 'above' },
        bottom: { left: midX, top: t.bottom + g, fits: t.bottom + g + h <= vh - 4, side: 'below' },
        left: { left: t.left - gapX - w, top: midY, fits: t.left - gapX - w >= 4, side: 'left' },
        right: { left: t.right + gapX, top: midY, fits: t.right + gapX + w <= vw - 4, side: 'right' },
      };
      for (const name of [first, opposite[first]]) {
        const at = spots[name];
        if (!at.fits) continue;
        box.dataset.side = at.side;
        box.style.left = `${Math.round(Math.max(4, Math.min(at.left, vw - w - 4)))}px`;
        box.style.top = `${Math.round(Math.max(4, Math.min(at.top, vh - h - 4)))}px`;
        return { l: t.left, r: t.right, t: t.top, b: t.bottom };
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
      at(pad.base + box.offsetHeight + gapOf(box));
    }
    function reveal(item, box) {
      const vh = item.ownerDocument.documentElement.clientHeight;
      const g = gapOf(box);
      if (item.getBoundingClientRect().bottom <= vh - box.offsetHeight - g) return;
      const was = item.style.scrollMarginBlockEnd;
      item.style.scrollMarginBlockEnd = `${box.offsetHeight + g}px`;
      item.scrollIntoView({ block: 'end', inline: 'nearest' });
      item.style.scrollMarginBlockEnd = was;
    }

    const registry = new Map();
    const drawn = new WeakMap();
    const modes = new WeakMap();
    const widths = new WeakMap();
    const anchors = new WeakMap();
    const watching = new WeakMap();
    function register(type, def, native = false) {
      const ok = tipText(type) && tipObject(def) && tipObject(def.shape);
      if (!ok || typeof def.flatten !== 'function' || typeof def.render !== 'function') {
        note(`register|${type}`, 'block(type, { shape, flatten, render }): a name, a shape object and two functions.');
        return false;
      }
      if (registry.get(type)?.native) {
        note(`register|${type}`, `block "${type}" is the engine's own and stays as it is.`);
        return false;
      }
      registry.set(type, { shape: def.shape, flatten: def.flatten, render: def.render, native });
      if (!native) for (const h of handles) h.redescribe();
      return true;
    }
    function block(type, def) {
      if (RESERVED.includes(type)) {
        note(
          `door|${type}`,
          `block("${type}"): the name belongs to an hovercraft block; it is not registered. Choose another.`,
        );
        return false;
      }
      return register(type, def);
    }
    const heading = (doc, kicker, title, meta) => {
      const node = el(doc, 'div', `${CSS}-title`, title);
      if (!kicker && !meta) return node;
      const wrap = el(doc, 'div', `${CSS}-heading`);
      if (kicker) wrap.append(el(doc, 'div', `${CSS}-kicker`, kicker));
      if (title) wrap.append(node);
      if (meta) wrap.append(el(doc, 'div', `${CSS}-meta`, meta));
      return wrap;
    };
    const native = (type, shape, flatten, render) =>
      register(type, { shape: { ...shape, parent: { type: 'string', required: false } }, flatten, render }, true);
    const joined = (...list) => list.filter((w) => w !== '').join(' ');
    native(
      'title',
      {
        text: { type: 'string', required: true },
        kicker: { type: 'string', required: false },
        meta: { type: 'string', required: false },
      },
      (b, _r, ctx) => joined(ctx.words(b.kicker), ctx.words(b.text), ctx.words(b.meta)),
      (b, _r, ctx) => ({ node: heading(ctx.doc, ctx.words(b.kicker), ctx.words(b.text), ctx.words(b.meta)) }),
    );
    native(
      'text',
      { text: { type: 'string', required: true } },
      (b, _r, ctx) => ctx.plain(b.text),
      (b, _r, ctx) => ({ node: ctx.write(el(ctx.doc, 'div', `${CSS}-text`), b.text) }),
    );
    native(
      'badge',
      { text: { type: 'string', required: true }, tone: { type: 'string', required: false } },
      (b, _r, ctx) => ctx.words(b.text),
      (b, _r, ctx) => {
        const badge = el(ctx.doc, 'span', `${CSS}-badge`, ctx.words(b.text));
        if (tipText(b.tone)) badge.dataset.tone = b.tone;
        return { node: badge };
      },
    );
    native(
      'fields',
      {
        fields: {
          type: 'list',
          required: true,
          min: 1,
          fields: { label: { type: 'string', required: true }, value: { type: 'scalar', required: true } },
        },
      },
      (b, _r, ctx) =>
        joined(
          ...(Array.isArray(b.fields) ? b.fields : [])
            .filter(tipObject)
            .map((f) => `${ctx.words(f.label)} ${ctx.words(f.value)}`.trim()),
        ),
      (b, _r, ctx) => {
        const dl = el(ctx.doc, 'dl', `${CSS}-fields`);
        for (const f of Array.isArray(b.fields) ? b.fields : []) {
          if (!tipObject(f)) continue;
          const row = el(ctx.doc, 'div');
          row.append(el(ctx.doc, 'dt', '', ctx.words(f.label)), el(ctx.doc, 'dd', '', ctx.words(f.value)));
          dl.append(row);
        }
        return { node: dl };
      },
    );
    native(
      'code',
      {
        field: { type: 'string', required: true },
        language: { type: 'string', required: false },
        copy: { type: 'boolean', required: false },
      },
      (b, record, ctx) => {
        const code = codeOf(record, b);
        if (code === null) return '';
        const lines = code.split(/\r\n|\r|\n/);
        const said = (lines.find((l) => l.trim() !== '') ?? '').trim();
        const first = said.length > 120 ? `${said.slice(0, 120).trimEnd()}…` : said;
        const key = `code_${lines.length === 1 ? 'line' : 'lines'}${first ? '' : '_empty'}`;
        return formatLabel((ctx?.labels ?? DEFAULT_LABELS)[key], { n: lines.length, first });
      },
      (b, record, ctx, more) => {
        const code = codeOf(record, b);
        if (code === null) return null;
        const { doc, pinned } = ctx;
        const { onCopy, code: at } = more;
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
          const button = el(doc, 'button', `${CSS}-copy`, (ctx.labels ?? DEFAULT_LABELS).copy);
          button.type = 'button';
          const status = el(doc, 'span', `${CSS}-status`);
          status.setAttribute('role', 'status');
          button.addEventListener('click', (e) => onCopy(code, b.field, at, status, e.detail === 0 ? 'key' : 'click'));
          bar.append(button, status);
          wrap.append(bar);
        }
        return { node: wrap };
      },
    );

    native(
      'group',
      {
        id: { type: 'string', required: true },
        direction: { type: 'enum', required: false, values: ['row', 'column'] },
        columns: { type: 'integer', required: false, min: 2, max: 4 },
        gap: { type: 'enum', required: false, values: ['s', 'm', 'l'] },
      },
      () => '',
      (b, _r, ctx) => {
        const node = el(ctx.doc, 'div', `${CSS}-group`);
        node.dataset.direction = b.direction === 'row' ? 'row' : 'column';
        if (['s', 'm', 'l'].includes(b.gap)) node.dataset.gap = b.gap;
        if (node.dataset.direction === 'row' && Number.isInteger(b.columns) && b.columns >= 2 && b.columns <= 4)
          node.dataset.columns = String(b.columns);
        return { node };
      },
    );

    const hosts = new Map();
    const LENGTH = /^(0|([0-9]+(\.[0-9]+)?|\.[0-9]+)(px|rem|em|vh|vw|%))$|^var\(--[A-Za-z0-9_-]+\)$/;
    const lengthOf = (v) => {
      if (typeof v === 'number') return Number.isFinite(v) && v >= 0 ? `${v}px` : null;
      return typeof v === 'string' && LENGTH.test(v) ? v : null;
    };
    function host(slot, handler) {
      const id = slotId(slot);
      const ok = tipObject(handler) && typeof handler.mount === 'function' && typeof handler.destroy === 'function';
      if (!id || !ok) {
        note(
          `host|door|${id}`,
          'host(slot, { mount, destroy, resize?, mode? }): a slot id and a handler with mount and destroy.',
        );
        return false;
      }
      hosts.set(id, handler);
      return true;
    }
    const jsonOf = (v) => {
      try {
        return JSON.parse(JSON.stringify(v));
      } catch {
        return {};
      }
    };
    function hostSlot(node, b) {
      const slot = slotId(b.id);
      node.setAttribute('data-hovercraft-host', slot);
      const room = lengthOf(b.height);
      if (room) node.style.minHeight = room;
      else node.style.removeProperty('min-height');
      if (!room && b.height !== undefined && b.height !== null)
        note(
          `host|height|${slot}`,
          `the host block "${slot}": height must be a length such as 12rem or 240 (pixels); it is left out.`,
        );
      return node;
    }
    native(
      'host',
      { id: { type: 'string', required: true }, height: { type: 'length', required: false } },
      () => '',
      (b, _r, ctx, more) => {
        const slot = slotId(b.id);
        if (!ctx.pinned) {
          note(`host|hint|${slot}`, `the host block "${slot}" is drawn in a pinned tip only; a hint leaves it out.`);
          return null;
        }
        const node = hostSlot(el(ctx.doc, 'div', `${CSS}-host`), b);
        let info = null;
        const mount = (root, { mode }) => {
          const h = hosts.get(slot);
          if (!h) return undefined;
          info = { slot, id: String(more.id), kind: more.kind, record: jsonOf(ctx.record), mode, pinned: true };
          h.mount(root, info);
          const life = {
            destroy: () => h.destroy(root, info),
            mode: (next) => {
              info.mode = next;
              if (typeof h.mode === 'function') h.mode(root, next);
            },
          };
          if (typeof h.resize === 'function') life.resize = () => h.resize(root, info);
          return life;
        };
        const renew = (record, id, kind, mode) => {
          if (info) Object.assign(info, { record: jsonOf(record), id: String(id), kind, mode });
        };
        return { node, mount, renew };
      },
    );

    const tryCall = (fn, ...args) => {
      try {
        return fn?.(...args);
      } catch (e) {
        note(`life|${e?.message}`, `a tooltip block failed to draw or let go: ${e?.message ?? e}`);
        return undefined;
      }
    };
    const lifeCall = (h, name, ...args) => tryCall((...a) => h.handle?.[name]?.(...a), ...args);
    function endLives(box, info) {
      const ending = drawn.get(box) ?? [];
      drawn.set(box, []);
      box.classList.remove(`${CSS}-wide`);
      if (info.reason !== 'redraw') watching.get(box)?.();
      for (const h of ending) lifeCall(h, 'destroy', info);
    }
    const widthOf = (box) => box.getBoundingClientRect().width;
    function startLives(box) {
      for (const h of drawn.get(box) ?? []) {
        if (h.mounted) continue;
        h.mounted = true;
        widths.set(box, widthOf(box));
        const handle = tryCall(h.mount, h.node, { box, mode: modes.get(box) ?? 'light' });
        if (tipObject(handle) || typeof handle === 'function') h.handle = handle;
      }
      watchMode(box);
    }
    function watchMode(box) {
      const anchor = anchors.get(box);
      const View = box.ownerDocument.defaultView;
      const was = watching.get(box);
      if (was && was.anchor === anchor) return;
      was?.();
      if (box.classList.contains(`${CSS}-pin`)) return;
      if (!anchor || typeof View?.MutationObserver !== 'function') return;
      const seen = new View.MutationObserver((records) => {
        copyMarks(box, anchors.get(box));
        if (records.some((r) => r.attributeName === ANCHOR_ATTR)) pageOf(box.ownerDocument).hint?.place(false);
      });
      for (let n = anchor; n?.nodeType === 1; n = n.parentElement)
        seen.observe(n, {
          attributes: true,
          attributeFilter: ['data-hovercraft-mode', 'data-bs-theme', CLASS_ATTR, ANCHOR_ATTR],
        });
      const stop = () => {
        seen.disconnect();
        watching.set(box, null);
      };
      stop.anchor = anchor;
      watching.set(box, stop);
    }
    const resizing = new WeakSet();
    const forced = new WeakSet();
    function resizeLives(box, force = false) {
      if (!(drawn.get(box) ?? []).some((h) => h.handle)) return;
      if (force) forced.add(box);
      if (resizing.has(box)) return;
      resizing.add(box);
      const run = () => {
        resizing.delete(box);
        const now = widthOf(box);
        if (!forced.delete(box) && now === widths.get(box)) return;
        widths.set(box, now);
        for (const h of drawn.get(box) ?? []) lifeCall(h, 'resize');
      };
      const view = box.ownerDocument.defaultView;
      if (typeof view?.requestAnimationFrame === 'function') view.requestAnimationFrame(run);
      else run();
    }
    function flipLives(box, next) {
      for (const h of drawn.get(box) ?? []) lifeCall(h, 'mode', next);
      resizeLives(box, true);
    }
    const LINK = /\[[^\]]*\]\([^)]+\)/;
    const pressable = (b) =>
      b.type === 'host' || (b.type === 'code' && b.copy === true) || (b.type === 'text' && LINK.test(String(b.text)));
    function withoutHosts(blocks) {
      let kept = blocks.filter((b) => b.type !== 'host');
      for (let again = true; again; ) {
        const used = new Set(kept.filter(hasParent).map((b) => String(b.parent)));
        const next = kept.filter((b) => b.type !== 'group' || used.has(String(b.id)));
        again = next.length !== kept.length;
        kept = next;
      }
      return kept;
    }
    function moreForm(content, pinned, phone) {
      const m = content.more;
      if (!m) return { hover: false, label: null };
      const hover = m.on === 'hover' && !(phone && m.canPin);
      if (pinned || m.cue !== 'icon' || !m.canPin) return { hover, label: null };
      return { hover, label: !hover ? 'more_pinned' : m.blocks.some(pressable) ? 'more' : null };
    }
    const SVG_NS = 'http://www.w3.org/2000/svg';
    const ELLIPSIS =
      '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false"><circle cx="12" cy="12" r="1"/><circle cx="19" cy="12" r="1"/><circle cx="5" cy="12" r="1"/></svg>';
    function cueNode(doc, words) {
      const line = el(doc, 'div', `${CSS}-cue`);
      line.innerHTML = ELLIPSIS;
      line.append(el(doc, 'span', '', words));
      return line;
    }
    function fill(box, content, { pinned, onCopy }) {
      const doc = box.ownerDocument;
      const want = pinned ? slotsOf(content) : [];
      const carried = new Map(
        (drawn.get(box) ?? []).filter((h) => h.mounted && want.includes(h.slot)).map((h) => [h.slot, h]),
      );
      drawn.set(
        box,
        (drawn.get(box) ?? []).filter((h) => !carried.has(h.slot)),
      );
      endLives(box, { reason: 'redraw', by: 'data', via: null });
      const stage = box.cloneNode(false);
      const marks = new Map();
      if (content.label) stage.append(el(doc, 'div', `${CSS}-title`, content.label));
      if (!content.blocks.length && content.reason) {
        stage.append(el(doc, 'div', `${CSS}-line ${CSS}-reason`, reasonWords(content)));
        box.replaceChildren(...stage.childNodes);
        return;
      }
      const ordered = headFirst(content.blocks);
      const first = ordered[0]?.type === 'title' && !hasParent(ordered[0]) ? ordered[0] : null;
      if (pinned) {
        const [kicker, title, meta] = ['kicker', 'text', 'meta'].map((k) => (first ? wordsOf(content, first[k]) : ''));
        const close = el(doc, 'button', `${CSS}-close`, '×');
        close.type = 'button';
        close.setAttribute('aria-label', content.labels.close);
        if (title || kicker || meta) {
          const head = el(doc, 'div', `${CSS}-head`);
          head.append(heading(doc, kicker, title, meta), close);
          stage.append(head);
        } else {
          close.classList.add(`${CSS}-corner`);
          stage.append(close);
        }
      }
      const kept = [];
      const mode = modes.get(box) ?? 'light';
      const ctxFor = (literal) => ({
        ...flatCtx(content, literal),
        doc,
        box,
        pinned,
        mode,
        write: (node, text) => {
          if (!node.parentNode) stage.append(node);
          env.markdown(node, markdownOf(content, text, literal), {
            field: 'tooltip',
            id: content.rec.id,
            notes: false,
          });
          return node;
        },
      });
      let code = -1;
      const draw = (n, into, top, ctx) => {
        const b = n.b;
        if (b.type === 'code') code += 1;
        const def = registry.get(b.type);
        if (!def || (top && pinned && b === first)) return;
        let h = null;
        let again = null;
        try {
          if (b.type === 'host') again = carried.get(slotId(b.id)) ?? null;
          if (again) {
            const mark = doc.createComment('host slot');
            marks.set(mark, again.node);
            h = { node: mark };
            hostSlot(again.node, b);
          } else if (def.native)
            h = def.render(b, content.record, ctx, {
              onCopy,
              code,
              id: content.rec.id,
              kind: content.rec.kind ?? 'row',
            });
          else h = def.render(b, content.record, ctx);
        } catch (e) {
          note(`render|${b.type}`, `a tooltip ${b.type} block could not be drawn: ${e?.message ?? e}`);
        }
        if (!h) return;
        if (h.node && h.node.parentNode !== into) into.append(h.node);
        if (n.kids.length && h.node) {
          const before = kept.length;
          for (const k of n.kids) draw(k, h.node, false, ctx);
          if (!h.node.firstChild) {
            note(`grp|empty|${b.id}`, `the group "${b.id}" drew no block; it is left out.`);
            h.node.remove();
            kept.length = before;
            return;
          }
        }
        if (again) {
          again.renew?.(content.record, content.rec.id, content.rec.kind ?? 'row', mode);
          kept.push(again);
        } else if (typeof h.mount === 'function' && h.node)
          kept.push({
            type: b.type,
            node: h.node,
            mount: h.mount,
            handle: null,
            mounted: false,
            slot: b.type === 'host' ? slotId(b.id) : undefined,
            renew: h.renew,
          });
      };
      const shortCtx = ctxFor(content.literal);
      for (const n of treeOf(ordered)) draw(n, stage, true, shortCtx);
      const phone = (doc.defaultView?.innerWidth ?? Number.POSITIVE_INFINITY) < PHONE;
      const form = moreForm(content, pinned, phone);
      if (content.more && (pinned || form.hover)) {
        const blocks = pinned ? content.more.blocks : withoutHosts(content.more.blocks);
        const wrap = pinned ? stage : el(doc, 'div', `${CSS}-more`);
        const longCtx = ctxFor(content.more.literal);
        for (const n of treeOf(blocks)) draw(n, wrap, false, longCtx);
        if (!pinned && wrap.firstChild) {
          wrap.setAttribute('tabindex', '-1');
          if (content.more.height) wrap.style.setProperty('--hovercraft-tip-more-max-height', content.more.height);
          stage.append(wrap);
        }
      }
      drawn.set(box, kept);
      if (!pinned && form.label) stage.append(cueNode(doc, content.labels[form.label]));
      if (content.reason) stage.append(el(doc, 'div', `${CSS}-line ${CSS}-reason`, reasonWords(content)));
      refill(box, stage, marks);
      if (!pinned) for (const n of box.querySelectorAll(PRESS)) n.tabIndex = -1;
      if (kept.length || box.querySelector(`.${CSS}-group[data-direction="row"]`)) box.classList.add(`${CSS}-wide`);
    }
    function refill(box, stage, marks) {
      if (!marks.size) {
        box.replaceChildren(...stage.childNodes);
        return;
      }
      const markIn = (n) => [...marks.keys()].find((m) => n.contains(m));
      const sync = (to, from) => {
        for (const a of [...to.attributes]) if (!from.hasAttribute(a.name)) to.removeAttribute(a.name);
        for (const a of from.attributes) if (to.getAttribute(a.name) !== a.value) to.setAttribute(a.name, a.value);
      };
      const shift = (into, t, at) => {
        let moved = false;
        if (typeof into.moveBefore === 'function' && t.isConnected && into.isConnected) {
          try {
            into.moveBefore(t, at);
            moved = true;
          } catch {
            moved = false;
          }
        }
        if (!moved) into.insertBefore(t, at);
      };
      const merge = (into, from) => {
        const stays = new Set();
        const targets = [];
        for (const c of [...from.childNodes]) {
          if (marks.has(c)) {
            targets.push(marks.get(c));
            stays.add(marks.get(c));
            continue;
          }
          const m = c.nodeType === 1 ? markIn(c) : null;
          const old = m ? [...into.children].find((o) => o.contains(marks.get(m)) && o.tagName === c.tagName) : null;
          if (old) {
            sync(old, c);
            merge(old, c);
            targets.push(old);
            stays.add(old);
          } else targets.push(c);
        }
        let at = into.firstChild;
        for (const t of targets) {
          if (t === at) {
            at = at.nextSibling;
            continue;
          }
          if (stays.has(t)) {
            while (at && at !== t && !stays.has(at)) {
              const next = at.nextSibling;
              at.remove();
              at = next;
            }
            if (at === t) {
              at = at.nextSibling;
              continue;
            }
          }
          shift(into, t, at);
        }
        while (at) {
          const next = at.nextSibling;
          if (!targets.includes(at)) at.remove();
          at = next;
        }
      };
      merge(box, stage);
      for (const [m, slot] of marks) {
        if (!m.isConnected) continue;
        const parent = m.parentNode;
        if (typeof parent.moveBefore === 'function' && slot.isConnected) {
          try {
            parent.moveBefore(slot, m);
          } catch {
            parent.insertBefore(slot, m);
          }
        } else parent.insertBefore(slot, m);
        m.remove();
      }
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
    const PRESS = 'button, a[href], summary, [role="button"]';
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
          page.hint.handle.hideHint(false, { reason: 'escape', by: 'reader', via: 'key' });
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
        if (page.hint) page.hint.place(false);
        if (page.pin) page.pin.place(false);
      };
      const onSize = () => {
        if (page.hint) page.hint.place();
        if (page.pin) page.pin.place();
        if (page.hint) resizeLives(boxesOf(doc).hint);
        if (page.pin) resizeLives(page.pin.box);
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
      view.addEventListener('resize', onSize);
      env.revealed?.(doc, () => page.pin?.check('reader'));
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
      const pinHow = (n) => (typeof pin === 'function' ? pin(n) : pin);
      const pinsAt = (n) => pinHow(n) === true || pinHow(n) === 'manual';
      const canPin = (n) => !disabledAt(n) && pinsAt(n);
      const partOf = (t) => t?.closest?.(PART) ?? null;
      const sideOf = () => (typeof side === 'function' ? side() : side);
      let timer = 0;
      let leave = 0;
      let awayOff = null;
      let seenHint = null;
      let stopHintWatch = null;
      let stopPinWatch = null;
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
        at.own_more_template =
          at.more_named_at !== null && (at.more_named_at === owner || owner.contains(at.more_named_at));
        return at;
      };
      const contentFor = (n) => {
        if (!when(n)) return null;
        const c = contentOf(recOf(n), settingsFor(n), {
          over: overOf(n),
          label: typeof label === 'function' ? label(n) : label,
        });
        if (c?.more) {
          if (c.more.on === 'pin' && !pinsAt(n)) {
            note(
              `more-pin|${selector}`,
              'a long form that shows when pinned needs an item that can pin; left out (pin it, or set tooltip_more_on to hover).',
            );
            c.more = null;
          } else c.more.canPin = canPin(n) && !focusOf(n)?.matches(':disabled');
        }
        return c;
      };
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
      const ARROW_ATTR = `${NAMES.attr}-arrow`;
      const arrowOf = (n) => {
        let on = null;
        for (let node = n; node && node.nodeType === 1 && on === null; node = node.parentElement) {
          const attr = node.getAttribute(ARROW_ATTR);
          if (attr !== null) on = attr.trim().toLowerCase() !== 'false';
          else if (own.get(node) && tipOwn(own.get(node), 'arrow')) on = own.get(node).arrow;
        }
        const name = presetAt(n);
        return on !== false && !(name && presetData(name).arrow === false);
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
        return d.id;
      };
      const madeFor = new WeakMap();
      const squash = (text) =>
        String(text ?? '')
          .replace(/\s+/g, ' ')
          .trim()
          .toLowerCase();
      const NAMED_BY_WORDS =
        'button, a[href], summary, h1, h2, h3, h4, h5, h6, [role="button"], [role="link"], [role="tab"], [role="menuitem"], [role="option"], [role="checkbox"], [role="radio"], [role="switch"], [role="treeitem"], [role="heading"]';
      const wordsIn = (n) => {
        const copy = n.cloneNode(true);
        for (const d of copy.querySelectorAll(`.${DESC}, [hidden], [aria-hidden="true"]`)) d.remove();
        return copy.textContent;
      };
      function nameOf(at) {
        const ids = (at.getAttribute('aria-labelledby') ?? '').split(/\s+/).filter(Boolean);
        const targets = ids.map((id) => doc.getElementById(id)).filter(Boolean);
        const named = squash(targets.map(wordsIn).join(' '));
        if (named) return named;
        const labelled = squash(at.getAttribute('aria-label'));
        return labelled || (at.matches(NAMED_BY_WORDS) ? squash(wordsIn(at)) : '');
      }
      function describeItem(item) {
        const n = focusOf(item);
        if (!when(item)) return;
        const c = contentFor(item);
        if (!c) return;
        const name = String(c.rec.label ?? '');
        const blocks = c.blocks.map((b) =>
          b.type === 'title' && name && wordsOf(c, b.text) === name ? { ...b, text: '' } : b,
        );
        const form = moreForm(c, false, false);
        const cue = form.label ? c.labels[form.label] : '';
        const plain = c.blocks.length ? plainOf({ ...c, label: '', blocks }, false, form.hover ? 'pin' : 'short') : '';
        const words = joined(plain, cue);
        const why = c.prefix ? reasonWords(c) : c.reason;
        const shared = sharedStop(item);
        const own = [];
        for (const at of shared ? [n, shared] : [n]) {
          const said = describedAs(at);
          if (words && !said.includes(words) && squash(words) !== nameOf(at))
            own.push({ at, id: addDesc(at, words, !!why && said.includes(why)) });
          if (why && !said.includes(why)) own.push({ at, id: addDesc(at, why) });
        }
        madeFor.set(item, own);
      }
      function describe() {
        for (const item of items()) {
          if (mine(focusOf(item))) continue;
          describeItem(item);
        }
        settle();
      }
      function redescribeItem(item) {
        for (const { at, id } of madeFor.get(item) ?? []) {
          doc.getElementById(id)?.remove();
          const keep = (at.getAttribute('aria-describedby') ?? '').split(/\s+/).filter((d) => d && d !== id);
          if (keep.length) at.setAttribute('aria-describedby', keep.join(' '));
          else at.removeAttribute('aria-describedby');
        }
        madeFor.delete(item);
        describeItem(item);
        settle();
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
      const itemWatchers = new Set();
      function settle() {
        watch.takeRecords();
        for (const w of itemWatchers) w.takeRecords();
      }
      const watch = new MutationObserver((records) => {
        const ours = records.every(
          (r) => [...r.addedNodes].every((x) => x.classList?.contains(DESC)) && !r.removedNodes.length,
        );
        if (ours) return;
        describe();
        if (page.hint?.handle === handle && !page.hint.anchor.isConnected) hideHint(false, goneInfo());
        checkOpen('data');
      });
      watch.observe(box, { childList: true, subtree: true });
      describe();

      const closeWhy = (reason, via = 'pointer') => ({ reason, by: 'reader', via });
      function hideHint(moved = false, why = { reason: 'leave', by: 'reader', via: 'pointer' }) {
        if (page.hint?.handle === handle && page.hint.held && HINT_HOLDS.includes(why.reason)) return false;
        clearTimeout(timer);
        clearTimeout(leave);
        timer = 0;
        awayOff?.();
        awayOff = null;
        seenHint?.disconnect();
        seenHint = null;
        stopHintWatch?.();
        stopHintWatch = null;
        inHint = false;
        if (page.hint?.handle !== handle) return false;
        const closing = page.hint;
        const delayed = closing.delayed;
        const info = hintWhy(why, lastPointer);
        page.hint = null;
        if (why.keep !== closing.anchor) unmarkOpen(closing.anchor, 'hint');
        if (moved !== true) closeSkip();
        kit.sizes?.unobserve(kit.hint);
        endLives(kit.hint, info);
        kit.hintMotion.set(false);
        unclass(kit.hint);
        if (moved === true && delayed) {
          page.skip = true;
          page.lastHint = closing.id;
          clearTimeout(page.skipTimer);
          page.skipTimer = setTimeout(() => {
            page.skip = false;
          }, HINT_DELAY);
        }
        emitHint('hidden', closing, info, closing.id);
        return true;
      }
      function emitHint(state, h, info, from) {
        env.emit(
          host,
          'hovercraft:hint',
          {
            state,
            id: h.id,
            kind: h.kind,
            text: h.text,
            by: info.by,
            via: info.via,
            from,
            reason: state === 'hidden' ? info.reason : null,
          },
          h.anchor,
        );
      }
      function hintCheck() {
        const h = page.hint;
        if (h?.handle !== handle) return;
        const n = h.anchor;
        const c = n.isConnected && n.getClientRects().length ? contentFor(n) : null;
        if (!c) {
          hideHint(false, goneInfo());
          return;
        }
        const sig = keyOf(c);
        if (sig === h.sig) return;
        h.sig = sig;
        h.text = cut(plainOf(c, true, 'hint'));
        fill(kit.hint, c, { pinned: false });
        redescribeItem(n);
        h.place();
        startLives(kit.hint);
      }
      function checkOpen(by) {
        if (page.hint?.handle === handle) {
          if (page.hint.anchor.isConnected && page.hint.anchor.getClientRects().length) {
            hintCheck();
            if (page.hint?.handle === handle) page.hint.place();
          } else hideHint(false, goneInfo());
        }
        if (page.pin?.handle === handle) page.pin.check(by);
      }
      function watchItem(n) {
        const View = doc.defaultView;
        if (!NAMES.standalone || typeof View?.MutationObserver !== 'function') return null;
        const seen = new View.MutationObserver((records) => {
          const ours = records.every(
            (r) =>
              r.type === 'childList' &&
              !r.removedNodes.length &&
              [...r.addedNodes].every((x) => x.classList?.contains(DESC)),
          );
          if (!ours) checkOpen('data');
        });
        seen.observe(n, {
          attributes: true,
          attributeFilter: [
            ATTR,
            `${ATTR}-template`,
            `${ATTR}-record`,
            `${ATTR}-reason`,
            `${ATTR}-more`,
            `${ATTR}-more-template`,
            MORE_ON_ATTR,
            MORE_CUE_ATTR,
          ],
          characterData: true,
          childList: true,
          subtree: true,
        });
        itemWatchers.add(seen);
        return () => {
          seen.disconnect();
          itemWatchers.delete(seen);
        };
      }
      function closeSkip() {
        page.skip = false;
        clearTimeout(page.skipTimer);
      }
      function showHint(n, how, who = 'reader') {
        if (page.pin?.handle === handle && page.pin.anchor() === n) return;
        const c = contentFor(n);
        if (!c) return;
        const resumed = page.skip ? (page.lastHint ?? null) : null;
        const again = page.hint?.handle === handle && page.hint.anchor === n;
        const held = how === 'command' || (again && page.hint.held === true);
        const before = again ? null : page.hint;
        if (before) before.handle.hideHint(false, { reason: 'replaced', by: who, via: how, keep: n });
        mountNear(kit.hint, n);
        looks(kit.hint, n);
        fill(kit.hint, c, { pinned: false });
        const s = sideOf();
        const x = how === 'pointer' || how === 'tap' ? xOf(n) : null;
        const by = besideOf(n);
        closeSkip();
        const text = cut(plainOf(c, true, 'hint'));
        page.hint = {
          handle,
          anchor: n,
          sig: keyOf(c),
          how,
          held,
          id: c.rec.id === undefined || c.rec.id === null || c.rec.id === '' ? null : String(c.rec.id),
          kind: c.rec.kind ?? 'row',
          text,
          delayed: c.blocks.length > 0,
          place: (restyle = true) => {
            if (restyle) copyMarks(kit.hint, n);
            place(kit.hint, anchorOf(n), s, x, by, atPointer(n) ? n : null, false, placementOf(n), arrowOf(n), n);
          },
        };
        markOpen(n, 'hint');
        kit.hintMotion.set(true);
        page.hint.place();
        kit.hintMotion.enter();
        startLives(kit.hint);
        kit.sizes?.observe(kit.hint);
        listenPage(doc);
        stopHintWatch?.();
        stopHintWatch = watchItem(n);
        seenHint?.disconnect();
        const view = doc.defaultView;
        seenHint =
          typeof view?.IntersectionObserver === 'function'
            ? new view.IntersectionObserver(() => {
                if (page.hint?.handle !== handle || page.hint.anchor !== n) return;
                if (!n.isConnected || !n.getClientRects().length) hideHint(false, goneInfo());
              })
            : null;
        seenHint?.observe(n);
        if (again) return;
        const info = hintWhy({ by: who, via: how }, lastPointer);
        emitHint('shown', page.hint, info, before ? before.id : resumed);
        if (how === 'command' && page.hint?.anchor === n) say(doc, plainOf(c, true, 'hint'), n);
      }
      function soon(n, how) {
        clearTimeout(timer);
        clearTimeout(leave);
        const c = contentFor(n);
        if (!c) return;
        if (page.hint?.handle === handle && page.hint.anchor === n) return;
        const open = page.hint?.delayed === true;
        const wait = c.blocks.length && !page.skip && !open ? delayAt(n) : 0;
        if (!wait) showHint(n, how);
        else timer = setTimeout(() => n.isConnected && showHint(n, how), wait);
      }
      const over = (e) => {
        if (e.pointerType === 'touch') return;
        const n = itemAt(e.target);
        if (!n || hitOwns(n)) return;
        hovered = true;
        clearTimeout(leave);
        if (page.hint?.handle === handle && page.hint.anchor !== n) hideHint(true, closeWhy('leave'));
        soon(n, 'pointer');
      };
      const out = (e) => {
        if (e.pointerType === 'touch') return;
        if (e.pointerType === 'pen' && page.hint?.handle === handle && page.hint.how === 'tap') return;
        const n = itemAt(e.target);
        if (!n || hitOwns(n) || n.contains(e.relatedTarget) || anchorOf(n).contains(e.relatedTarget)) return;
        hovered = false;
        clearTimeout(timer);
        if (kit.hint.contains(e.relatedTarget)) return;
        leave = setTimeout(() => hideHint(true, closeWhy('leave')), LEAVE_MS);
      };
      const hintOver = () => {
        if (page.hint?.handle !== handle) return;
        clearTimeout(leave);
        inHint = true;
      };
      const hintOut = (e) => {
        if (page.hint?.handle !== handle || kit.hint.contains(e.relatedTarget) || partOf(e.relatedTarget)) return;
        if (e.pointerType === 'touch' || page.hint.how === 'tap') return;
        const n = page.hint.anchor;
        if (n.contains(e.relatedTarget) || anchorOf(n).contains(e.relatedTarget)) return;
        inHint = false;
        if (e.buttons & 1) {
          const up = (ev) => {
            if (page.hint?.handle !== handle || kit.hint.contains(ev.target) || anchorOf(n).contains(ev.target)) return;
            const sel = doc.getSelection?.();
            if (sel && String(sel) && kit.hint.contains(sel.anchorNode)) {
              doc.addEventListener(
                'pointerdown',
                (d) => !kit.hint.contains(d.target) && hideHint(true, closeWhy('outside')),
                {
                  capture: true,
                  once: true,
                },
              );
            } else hideHint(true, closeWhy('leave'));
          };
          doc.addEventListener('pointerup', up, { capture: true, once: true });
          return;
        }
        leave = setTimeout(() => hideHint(true, closeWhy('leave')), LEAVE_MS);
      };
      const hintDown = (e) => {
        if (e.target.closest?.(PRESS)) e.preventDefault();
      };
      const focus = (e) => {
        const n = itemAt(e.target);
        const stop = typeof focusAt !== 'function' || (n && e.target === focusOf(n));
        if (n && stop && e.target.matches(':focus-visible')) soon(n, 'focus');
        else if (!n && !partOf(e.target)) hideHint(true, closeWhy('blur', 'focus'));
      };
      const blur = (e) => {
        clearTimeout(timer);
        if (page.hint?.handle !== handle) return;
        if (kit.hint.contains(e.relatedTarget) || partOf(e.relatedTarget) || inHint) return;
        if (hovered && page.hint.how === 'pointer') return;
        hideHint(true, closeWhy('blur', 'focus'));
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
        const touched = lastPointer !== 'mouse';
        if (!reasonOnly && (!touched || (canPin(n) && pinHow(n) !== 'manual'))) return;
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
          if (!acts(n)) hideHint(false, closeWhy('leave', 'tap'));
          return;
        }
        showHint(n, 'tap');
        if (page.hint?.handle !== handle || page.hint.anchor !== n) return;
        awayOff?.();
        const away = (ev) => {
          if (partOf(ev.target)) return;
          if (kit.hint.contains(ev.target)) {
            if (![...kit.hint.querySelectorAll(PRESS)].some((n) => n.getClientRects().length && !n.disabled)) {
              hideHint(false, closeWhy('outside', 'tap'));
              swallowClick();
            }
            return;
          }
          if (itemAt(ev.target) === n) return;
          hideHint(false, closeWhy('outside', 'tap'));
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
            hideHint(false, { reason: 'escape', by: 'reader', via: 'key' });
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
          const stop = () =>
            (owner === doc.documentElement ? null : owner.querySelector('[tabindex="0"]')) ?? page.pin.focusAt();
          const to =
            at >= 0 && at < stops.length
              ? stops[at]
              : e.shiftKey
                ? page.pin.focusAt()
                : (page.pin.after() ?? (page.pin.box.parentElement?.closest(MODAL) ? stop() : null));
          if (!to) return;
          to.focus();
          if (doc.activeElement === to) used(e);
        }
      };

      const pinned = () => (page.pin?.handle === handle ? page.pin.id : null);
      function emitTip(state, id, kind, reason, text, info, from, item = null, slots = []) {
        env.emit(
          host,
          'hovercraft:tip',
          {
            state,
            id,
            kind,
            ids: state === 'closed' ? [] : [id],
            reason,
            text,
            by: info.by ?? 'host',
            from,
            via: info.via ?? null,
            slots,
          },
          item,
        );
      }
      function announce(p) {
        if (p.frame) return;
        const send = () => {
          p.frame = 0;
          if (page.pin !== p) return;
          emitTip('updated', p.id, p.kind, null, p.text, { by: 'data', via: null }, p.id, p.item(), p.slots);
        };
        const view = doc.defaultView;
        if (typeof view?.requestAnimationFrame === 'function') p.frame = view.requestAnimationFrame(send);
        else send();
      }
      function cancelFrame(p) {
        if (p.frame) doc.defaultView?.cancelAnimationFrame?.(p.frame);
        p.frame = 0;
      }
      function closePin(info = {}, refocus = false) {
        const p = page.pin;
        if (!p || p.handle !== handle) return false;
        const anchor = p.focusAt();
        page.pin = null;
        cancelFrame(p);
        p.stop();
        const hadFocus = p.box.contains(doc.activeElement);
        kit.sizes?.unobserve(p.box);
        room(p.box, false);
        endLives(p.box, { reason: info.reason ?? 'host', by: info.by ?? 'host', via: info.via ?? null });
        kit.pinMotion.set(false);
        unclass(p.box);
        emitTip('closed', p.id, p.kind, info.reason ?? 'host', p.text, info, p.id, p.item(), p.slots);
        for (const slot of p.box.querySelectorAll(`.${CSS}-host`)) slot.remove();
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
        if (page.hint) page.hint.handle.hideHint(false, { reason: 'pinned', by, via: info.via ?? null, keep: n });
        const rec = c.rec;
        const kind = rec.kind ?? 'row';
        const text = cut(plainOf(c));
        const name = titleOf(c) || String(rec.label ?? key);
        const s = sideOf();
        const x = info.via === 'click' ? xOf(n) : null;
        const pbox = kit.pin;
        mountNear(pbox, n);
        looks(pbox, n);
        const onCopy = (code, field, block, status, via) =>
          copyText(doc, code, pbox, (ok) => {
            const item = page.pin?.item() ?? n;
            env.emit(host, 'hovercraft:copy', { id: key, kind, field, block, ok, by: 'reader', via }, item);
            status.textContent = '';
            clearTimeout(status.tipTimer);
            setTimeout(() => {
              status.textContent = labelsAt(item)[ok ? 'copied' : 'copy_failed'];
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
          if (e === born || pbox.contains(e.target) || partOf(e.target) || (hit && canPin(hit))) return;
          closePin({ reason: 'outside', by: 'reader', via: 'click' });
        });
        const inside = (e) => {
          const hit = itemAt(e.target);
          if (e === born || (hit && canPin(hit)) || pbox.contains(e.target) || partOf(e.target)) return;
          if (e.target.closest?.('[data-hovercraft-keep]')) return;
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
        const onModalEscape = (e) => {
          used(e);
          closePin({ reason: 'escape', by: 'reader', via: 'key' }, true);
        };
        const offModal = inModal(pbox, () => page.pin?.handle === handle, onAround, onModalEscape);
        let anchorEl = n;
        let marked = n;
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
        const WATCHED = [
          ...MARKS,
          'data-hovercraft-mode',
          'data-bs-theme',
          CLASS_ATTR,
          ANCHOR_ATTR,
          'class',
          'style',
          'hidden',
        ];
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
          sig: keyOf(c),
          frame: 0,
          slots: slotsOf(c),
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
              if (anchorEl && anchorEl !== was) {
                unmarkOpen(marked, 'pin');
                marked = anchorEl;
                markOpen(marked, 'pin');
                watchLine(anchorEl);
                stopPinWatch?.();
                stopPinWatch = watchItem(anchorEl);
              }
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
            const perItem = typeof focusAt === 'function' || owner === doc.documentElement;
            return nextAfter(doc, perItem && stop ? stop : owner);
          },
          place: (restyle = true) => {
            const a = page.pin?.anchor();
            if (!a) return;
            if (restyle) copyMarks(pbox, a);
            place(pbox, anchorOf(a), s, x, pinBeside(a), atPointer(a) ? a : null, true, placementOf(a), arrowOf(a), a);
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
              const slots = slotsOf(fresh);
              const old = page.pin.slots;
              const same = slots.length === old.length && slots.every((id) => old.includes(id));
              const sig = keyOf(fresh);
              if (sig !== page.pin.sig || !same) {
                const p = page.pin;
                const data = { reason: 'redraw', by: 'data', via: null };
                if (!same) {
                  cancelFrame(p);
                  endLives(pbox, data);
                  emitTip('closed', key, kind, 'redraw', p.text, data, key, a, old);
                  if (page.pin !== p) return;
                }
                p.sig = sig;
                p.text = cut(plainOf(fresh));
                p.slots = slots;
                fill(pbox, fresh, { pinned: true, onCopy });
                pbox.setAttribute('aria-label', titleOf(fresh) || String(fresh.rec.label ?? key));
                pbox.querySelector(`.${CSS}-close`)?.addEventListener('click', onClose);
                stopsOff(pbox);
                redescribeItem(a);
                place(
                  pbox,
                  anchorOf(a),
                  s,
                  x,
                  pinBeside(a),
                  atPointer(a) ? a : null,
                  true,
                  placementOf(a),
                  arrowOf(a),
                  a,
                );
                startLives(pbox);
                if (same && slots.length) resizeLives(pbox, true);
                if (!same) {
                  emitTip('pinned', key, kind, null, p.text, { by: 'data', via: null }, key, a, slots);
                  if (page.pin !== p) return;
                } else announce(p);
              }
              page.pin?.place();
            }
          },
          stop: () => {
            unmarkOpen(marked, 'pin');
            stopPinWatch?.();
            stopPinWatch = null;
            seen?.disconnect();
            marks.disconnect();
            offOutside();
            owner.removeEventListener('click', inside);
            pbox.removeEventListener('keydown', tipKeys);
            around?.removeEventListener('close', onAround);
            offModal();
            stack?.remove(entry);
          },
        };
        markOpen(n, 'pin');
        kit.pinMotion.set(true);
        stopPinWatch?.();
        stopPinWatch = watchItem(n);
        stopsOff(pbox);
        page.pin.place();
        if (pbox.dataset.side === 'sheet') reveal(n, pbox);
        startLives(pbox);
        kit.sizes?.observe(pbox);
        listenPage(doc);
        emitTip('pinned', key, kind, null, text, { by, via: info.via ?? null }, from, n, page.pin.slots);
        say(doc, formatLabel(c.labels.pinned, { label: name }), n);
        kit.pinMotion.enter();
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
      kit.hint.addEventListener('mousedown', hintDown);
      const partOver = (e) => {
        if (partOf(e.target)) hintOver();
      };
      const partOut = (e) => {
        if (partOf(e.target)) hintOut(e);
      };
      doc.addEventListener('pointerover', partOver);
      doc.addEventListener('pointerout', partOut);

      function hideByHost() {
        return hideHint(false, hostInfo());
      }
      function showByHost(el) {
        if (el?.nodeType !== 1) {
          note('show|element', 'show(el): el must be an element.');
          return false;
        }
        if (!el.isConnected) {
          note('show|connected', 'show(el): el is not in the page.');
          return false;
        }
        const n = itemAt(el);
        if (!n) {
          note('show|item', 'show(el): el is not an item of this part.');
          return false;
        }
        if (page.pin?.handle === handle && page.pin.anchor() === n) return true;
        if (page.hint?.handle === handle && page.hint.anchor === n) {
          page.hint.held = true;
          return true;
        }
        if (!contentFor(n)) {
          note('show|tip', 'show(el): el has no tip to show.');
          return false;
        }
        if (!n.getClientRects().length) {
          note('show|drawn', 'show(el): el is not drawn (hidden), so it has no place for a tip.');
          return false;
        }
        clearTimeout(timer);
        clearTimeout(leave);
        showHint(n, 'command', 'host');
        return page.hint?.handle === handle && page.hint.anchor === n;
      }
      function hitOwns(n) {
        return NAMES.standalone && n.matches(':disabled') && !!recOf(n)?.reason;
      }
      function hitOn(n) {
        hovered = true;
        lastPointer = 'mouse';
        clearTimeout(leave);
        if (page.hint?.handle === handle && page.hint.anchor !== n) hideHint(true, closeWhy('leave'));
        soon(n, 'pointer');
      }
      function hitOff(n) {
        hovered = false;
        clearTimeout(timer);
        clearTimeout(leave);
        const open = page.hint;
        if (open?.handle !== handle || open.anchor !== n || open.how === 'tap') return;
        leave = setTimeout(() => hideHint(true, closeWhy('leave')), LEAVE_MS);
      }
      function hitTap(n, type) {
        lastPointer = type;
        if (contentFor(n)) tapOn(n, type !== 'mouse');
      }

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
          checkOpen('data');
        },
        refresh(info = {}) {
          describe();
          checkOpen(info.by ?? 'data');
        },
        hide: hideByHost,
        show: showByHost,
        hitOn,
        hitOff,
        hitTap,
        hitOwns,
        destroy() {
          if (gone) return;
          gone = true;
          handles.delete(handle);
          hideHint(false, goneInfo());
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
          kit.hint.removeEventListener('mousedown', hintDown);
          doc.removeEventListener('pointerover', partOver);
          doc.removeEventListener('pointerout', partOut);
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
        hide: hideByHost,
        show: showByHost,
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

    function reasonTips(box, { selector, when = () => true, side = 'below', host = null } = {}) {
      const t = tips(host ?? { root: box }, { box, selector, when, side, root: box });
      return Object.freeze({ hide: t.hide, destroy: t.destroy });
    }

    const ATTR = NAMES.attr;
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
      const GIVEN = '[data-reason][aria-disabled="true"], [data-reason]:disabled';
      const selector = `[${ATTR}], [${ATTR}-template], [${ATTR}-more], [${ATTR}-more-template], [${REASON}], ${GIVEN}, ${PIN}`;
      const ours = (n) => !(NAMES.skip && n.closest(NAMES.skip)) && !n.matches(BSLIB);
      const reasonAt = (n) => {
        const why = n.getAttribute(REASON) ?? (n.matches(GIVEN) ? n.dataset.reason : null);
        return tipText(why) ? why : null;
      };
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
      const byHand = (n) => n.getAttribute(PIN_ATTR) === 'manual';
      const picks = (n) => pinnable(n) && !byHand(n);
      const marked = new WeakSet();
      const managed = new WeakSet();
      const natives = new Set();
      const t = tips(
        { root: at },
        {
          box: at,
          selector,
          pin: (n) => pinnable(n) && (byHand(n) ? 'manual' : true),
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
            const long = n.getAttribute(`${ATTR}-more`);
            if (tipText(long)) row = { ...row, tooltip_more: long };
            const longName = n.getAttribute(`${ATTR}-more-template`);
            const held = own.get(n);
            if (longName && !tipText(long)) own.set(n, { ...(held ?? {}), tooltip_more_template: longName });
            else if (held && tipOwn(held, 'tooltip_more_template')) delete held.tooltip_more_template;
            const reason = reasonAt(n);
            if (reason && n.matches(':disabled')) {
              managed.add(n);
              natives.add(n);
              if (marked.has(n)) {
                n.removeAttribute('aria-disabled');
                marked.delete(n);
              }
            } else natives.delete(n);
            if (reason && n.getAttribute('aria-disabled') !== 'true' && !managed.has(n)) {
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
        if (n && at.contains(n) && picks(n)) flip(n, e.detail === 0 ? 'key' : 'click');
      };
      const onKey = (e) => {
        if ((e.key !== 'Enter' && e.key !== ' ') || e.isComposing || e.defaultPrevented) return;
        if (e.target.matches?.('button')) return;
        const n = pinAt(e.target);
        if (!n || !at.contains(n) || !picks(n)) return;
        e.preventDefault();
        flip(n, 'key');
      };
      const blocked = (target) => {
        const n = target?.closest?.(`[${REASON}]`) ?? h.itemAt(target);
        return n && at.contains(n) && ours(n) && !managed.has(n) && tipText(n.getAttribute(REASON)) ? n : null;
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
      const NAME_ATTRS = ['aria-label', 'aria-labelledby'];
      const reasons = new MutationObserver((records) => {
        if (records.every((r) => NAME_ATTRS.includes(r.attributeName) && !h.itemAt(r.target))) return;
        for (const r of records) {
          const n = r.target;
          if (!tipText(n.getAttribute(REASON)) && marked.has(n)) {
            n.removeAttribute('aria-disabled');
            marked.delete(n);
          }
          if (!reasonAt(n)) {
            managed.delete(n);
            natives.delete(n);
          }
        }
        h.redescribe();
      });
      reasons.observe(at, {
        attributes: true,
        subtree: true,
        attributeFilter: [REASON, 'data-reason', 'aria-disabled', 'disabled', ...NAME_ATTRS],
      });
      const doc = at.ownerDocument;
      const view = doc.defaultView;
      let hit = null;
      let frame = 0;
      let lastMove = null;
      const underAt = (e) => {
        const top = doc.elementsFromPoint?.(e.clientX, e.clientY)?.[0] ?? null;
        const n = top?.closest?.(selector) ?? null;
        return { top, item: n && at.contains(n) && ours(n) && natives.has(n) && h.hitOwns(n) ? n : null };
      };
      const probe = (e) => {
        for (const n of natives) if (!n.isConnected) natives.delete(n);
        if (!natives.size && !hit) return;
        const { top, item } = underAt(e);
        if (item) {
          hit = item;
          h.hitOn(item);
        } else if (hit && !(top && boxesOf(doc).hint.contains(top))) {
          const was = hit;
          hit = null;
          h.hitOff(was);
        }
      };
      const onMove = (e) => {
        if (e.pointerType === 'touch') return;
        lastMove = e;
        if (frame) return;
        if (typeof view?.requestAnimationFrame !== 'function') probe(e);
        else
          frame = view.requestAnimationFrame(() => {
            frame = 0;
            probe(lastMove);
          });
      };
      const onOut = (e) => {
        if (e.relatedTarget || !hit) return;
        const was = hit;
        hit = null;
        h.hitOff(was);
      };
      const onUp = (e) => {
        if (e.pointerType !== 'touch' && e.pointerType !== 'pen') return;
        const { item } = underAt(e);
        if (item) h.hitTap(item, e.pointerType);
      };
      doc.addEventListener('pointermove', onMove, true);
      doc.addEventListener('pointerout', onOut, true);
      doc.addEventListener('pointerup', onUp, true);
      at.addEventListener('click', stopClick, true);
      at.addEventListener('keydown', stopKey, true);
      at.addEventListener('click', onClick);
      at.addEventListener('keydown', onKey);
      const door = Object.freeze({
        show: (el) => t.show(el),
        hide: () => t.hide(),
        pin: (el, opts) => {
          const n = h.itemAt(el);
          if (!n) {
            note('pin|item', 'pin(el): el has no tip to pin.');
            return false;
          }
          const no = n.hasAttribute(PIN_ATTR) ? refusal(n) : `it has no ${PIN_ATTR}`;
          if (no || !pinnable(n)) {
            note(`pin|${no}`, `pin(el): it cannot pin: ${no ?? "it is not one of this scope's tips"}.`);
            return false;
          }
          if (t.pinned === idOf(n)) return true;
          const ok = h.pinElement(n, pinWho(opts));
          if (!ok && n.getAttribute('aria-disabled') !== 'true')
            note('pin|tip', 'pin(el): its tip has no content to pin.');
          return ok;
        },
        close: () => t.close({ reason: 'host', by: 'host', via: 'command' }),
      });
      doors.set(at, door);
      liveDoors.add(door);
      const handle = Object.freeze({
        destroy: () => {
          attached.delete(at);
          doors.delete(at);
          liveDoors.delete(door);
          reasons.disconnect();
          if (frame) view?.cancelAnimationFrame?.(frame);
          doc.removeEventListener('pointermove', onMove, true);
          doc.removeEventListener('pointerout', onOut, true);
          doc.removeEventListener('pointerup', onUp, true);
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
    const doors = new WeakMap();
    const liveDoors = new Set();
    function doorFor(el, what) {
      if (el?.nodeType !== 1) {
        note(`${what}|element`, `${what}(el): el must be an element.`);
        return null;
      }
      if (!el.isConnected) {
        note(`${what}|connected`, `${what}(el): el is not in the page.`);
        return null;
      }
      for (let node = el; node; node = node.parentElement) if (doors.has(node)) return doors.get(node);
      note(`${what}|scope`, `${what}(el): el is under no scope attach() was given.`);
      return null;
    }
    const commandShow = (el) => doorFor(el, 'show')?.show(el) ?? false;
    function pinWho(o) {
      const { by = 'host', via = 'command' } = o ?? {};
      if (typeof (o ?? {}) === 'object' && /^(reader|host)$/.test(by) && /^(click|key|command)$/.test(via))
        return { by, via };
      note('pin|who', 'pin(el, { by, via }): by is reader or host, via is click, key or command.');
      return { by: 'host', via: 'command' };
    }
    const commandPin = (el, opts) => doorFor(el, 'pin')?.pin(el, opts) ?? false;
    const commandHide = () => [...liveDoors].map((d) => d.hide()).includes(true);
    const commandClose = () => [...liveDoors].map((d) => d.close()).includes(true);

    return Object.freeze({
      tips,
      reasonTips,
      tooltip,
      define,
      attach,
      show: commandShow,
      hide: commandHide,
      pin: commandPin,
      close: commandClose,
      block,
      host,
      register,
      blocks: () => [...registry].map(([type, { shape, native }]) => ({ type, shape, native })),
      contentOf,
      plainOf,
      groupDepth: GROUP_DEPTH,
      settingsAt,
      placements: [...PLACEMENTS],
      anchors: [...ANCHORS],
      presets: [...Object.keys(PRESET_TEXT), PRESET_NONE],
      moreOn: [...MORE_ON],
      moreCue: [...MORE_CUE],
    });
  }

  const engine = createTipEngine({
    emit: (host, type, detail, item) => {
      const at = item?.isConnected ? item : host.root?.isConnected ? host.root : (item ?? host.root)?.ownerDocument;
      at?.dispatchEvent(new CustomEvent(type, { bubbles: true, detail }));
    },
    markdown,
    num,
    format: format_number,
    layer: () => null,
    motion: (el, o = {}) => {
      let a = null;
      const clear = () => {
        a = null;
      };
      const stop = () => {
        a?.cancel?.();
        clear();
      };
      const mq = el.ownerDocument.defaultView?.matchMedia?.('(prefers-reduced-motion: reduce)');
      mq?.addEventListener?.('change', (e) => {
        if (e.matches) stop();
      });
      const S = { left: 'X(-4', right: 'X(4', top: 'Y(-4', bottom: 'Y(4' };
      return {
        set: (open) => {
          stop();
          el.hidden = !open;
        },
        enter: () => {
          stop();
          if (el.hidden || !el.isConnected || mq?.matches || !el.animate) return;
          const ax = typeof o.axis === 'function' ? o.axis() : o.axis;
          if (ax !== 'fade' && ax !== 'slide') return;
          const s = ax === 'slide' ? S[typeof o.side === 'function' ? o.side() : o.side] : '';
          a = el.animate(
            [
              { opacity: 0, transform: s ? `translate${s}px)` : 'none' },
              { opacity: 1, transform: 'none' },
            ],
            { duration: 120, easing: 'ease-out' },
          );
          a.finished.then(clear, clear);
        },
      };
    },
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
    block: engine.block,
    host: engine.host,
    show: engine.show,
    hide: engine.hide,
    pin: engine.pin,
    close: engine.close,
  });
})();
