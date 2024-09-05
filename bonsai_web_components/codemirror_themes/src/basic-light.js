// Colors from https://www.nordtheme.com/docs/colors-and-palettes
// Polar Night
var base00 = '#2e3440',
    // black
base01 = '#3b4252',
    // dark grey
base02 = '#434c5e',
    base03 = '#4c566a'; // grey
// Snow Storm

var base04 = '#d8dee9',
    // grey
base05 = '#e5e9f0',
    // off white
base06 = '#eceff4'; // white
// Frost

var base07 = '#8fbcbb',
    // moss green
base08 = '#88c0d0',
    // ice blue
base09 = '#81a1c1',
    // water blue
base0A = '#5e81ac'; // deep blue
// Aurora

var base0b = '#bf616a',
    // red
base0C = '#d08770',
    // orange
base0D = '#ebcb8b',
    // yellow
base0E = '#a3be8c',
    // green
base0F = '#b48ead'; // purple

var invalid = '#d30102',
    darkBackground = base06,
    highlightBackground = darkBackground,
    background = '#ffffff',
    tooltipBackground = base01,
    selection = darkBackground,
    cursor = base01; /// The editor theme styles for Basic Light.

var basicLightTheme = {
  '&': {
    color: base00,
    backgroundColor: background
  },
  '.cm-content': {
    caretColor: cursor
  },
  '.cm-cursor, .cm-dropCursor': {
    borderLeftColor: cursor
  },
  '&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection': {
    backgroundColor: selection
  },
  '.cm-panels': {
    backgroundColor: darkBackground,
    color: base03
  },
  '.cm-panels.cm-panels-top': {
    borderBottom: '2px solid black'
  },
  '.cm-panels.cm-panels-bottom': {
    borderTop: '2px solid black'
  },
  '.cm-searchMatch': {
    backgroundColor: '#72a1ff59',
    outline: "1px solid ".concat(base03)
  },
  '.cm-searchMatch.cm-searchMatch-selected': {
    backgroundColor: base05
  },
  '.cm-activeLine': {
    backgroundColor: highlightBackground
  },
  '.cm-selectionMatch': {
    backgroundColor: base05
  },
  '&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket': {
    outline: "1px solid ".concat(base03)
  },
  '&.cm-focused .cm-matchingBracket': {
    backgroundColor: base06
  },
  '.cm-gutters': {
    backgroundColor: base06,
    color: base00,
    border: 'none'
  },
  '.cm-activeLineGutter': {
    backgroundColor: highlightBackground
  },
  '.cm-foldPlaceholder': {
    backgroundColor: 'transparent',
    border: 'none',
    color: '#ddd'
  },
  '.cm-tooltip': {
    border: 'none',
    backgroundColor: tooltipBackground
  },
  '.cm-tooltip .cm-tooltip-arrow:before': {
    borderTopColor: 'transparent',
    borderBottomColor: 'transparent'
  },
  '.cm-tooltip .cm-tooltip-arrow:after': {
    borderTopColor: tooltipBackground,
    borderBottomColor: tooltipBackground
  },
  '.cm-tooltip-autocomplete': {
    '& > ul > li[aria-selected]': {
      backgroundColor: highlightBackground,
      color: base03
    }
  }
}


globalThis.cm6_themes_basicLightTheme = basicLightTheme;

function basicLightHighlightStyle() {
  var _highlight = codemirror.Lezer_highlight;
  return [{
  tag: _highlight.tags.keyword,
  color: base0A
}, {
  tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.propertyName, _highlight.tags.macroName],
  color: base0C
}, {
  tag: [_highlight.tags.variableName],
  color: base0C
}, {
  tag: [_highlight.tags["function"](_highlight.tags.variableName)],
  color: base0A
}, {
  tag: [_highlight.tags.labelName],
  color: base09
}, {
  tag: [_highlight.tags.color, _highlight.tags.constant(_highlight.tags.name), _highlight.tags.standard(_highlight.tags.name)],
  color: base0A
}, {
  tag: [_highlight.tags.definition(_highlight.tags.name), _highlight.tags.separator],
  color: base0E
}, {
  tag: [_highlight.tags.brace],
  color: base07
}, {
  tag: [_highlight.tags.annotation],
  color: invalid
}, {
  tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
  color: base08
}, {
  tag: [_highlight.tags.typeName, _highlight.tags.className],
  color: base0D
}, {
  tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
  color: base0E
}, {
  tag: [_highlight.tags.tagName],
  color: base0F
}, {
  tag: [_highlight.tags.squareBracket],
  color: base0b
}, {
  tag: [_highlight.tags.angleBracket],
  color: base0C
}, {
  tag: [_highlight.tags.attributeName],
  color: base0D
}, {
  tag: [_highlight.tags.regexp],
  color: base0A
}, {
  tag: [_highlight.tags.quote],
  color: base01
}, {
  tag: [_highlight.tags.string],
  color: base0C
}, {
  tag: _highlight.tags.link,
  color: base07,
  textDecoration: 'underline',
  textUnderlinePosition: 'under'
}, {
  tag: [_highlight.tags.url, _highlight.tags.escape, _highlight.tags.special(_highlight.tags.string)],
  color: base0C
}, {
  tag: [_highlight.tags.meta],
  color: base08
}, {
  tag: [_highlight.tags.comment],
  color: base02,
  fontStyle: 'italic'
}, {
  tag: _highlight.tags.strong,
  fontWeight: 'bold',
  color: base0A
}, {
  tag: _highlight.tags.emphasis,
  fontStyle: 'italic',
  color: base0A
}, {
  tag: _highlight.tags.strikethrough,
  textDecoration: 'line-through'
}, {
  tag: _highlight.tags.heading,
  fontWeight: 'bold',
  color: base0A
}, {
  tag: _highlight.tags.special(_highlight.tags.heading1),
  fontWeight: 'bold',
  color: base0A
}, {
  tag: _highlight.tags.heading1,
  fontWeight: 'bold',
  color: base0A
}, {
  tag: [_highlight.tags.heading2, _highlight.tags.heading3, _highlight.tags.heading4],
  fontWeight: 'bold',
  color: base0A
}, {
  tag: [_highlight.tags.heading5, _highlight.tags.heading6],
  color: base0A
}, {
  tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
  color: base0C
}, {
  tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted],
  color: base07
}, {
  tag: [_highlight.tags.contentSeparator],
  color: base0D
}, {
  tag: _highlight.tags.invalid,
  color: base02,
  borderBottom: "1px dotted ".concat(invalid)
}]
}

globalThis.cm6_themes_basicLightHighlightStyle = basicLightHighlightStyle;

