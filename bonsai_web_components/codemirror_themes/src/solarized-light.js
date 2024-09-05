var base00 = '#657b83',
    base01 = '#586e75',
    base02 = '#073642',
    base03 = '#002b36',
    base0 = '#839496',
    base1 = '#93a1a1',
    base2 = '#eee8d5',
    base3 = '#fdf6e3',
    base_red = '#dc322f',
    base_orange = '#cb4b16',
    base_yellow = '#b58900',
    base_green = '#859900',
    base_cyan = '#2aa198',
    base_blue = '#268bd2',
    base_violet = '#6c71c4',
    base_magenta = '#d33682';
var invalid = '#d30102',
    darkBackground = '#dfd9c8',
    highlightBackground = darkBackground,
    background = base3,
    tooltipBackground = base01,
    selection = darkBackground,
    cursor = base01; /// The editor theme styles for Solarized Light.

var solarizedLightTheme = {
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
    outline: '1px solid #457dff'
  },
  '.cm-searchMatch.cm-searchMatch-selected': {
    backgroundColor: '#6199ff2f'
  },
  '.cm-activeLine': {
    backgroundColor: highlightBackground
  },
  '.cm-selectionMatch': {
    backgroundColor: '#aafe661a'
  },
  '&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket': {
    outline: "1px solid ".concat(base1)
  },
  '.cm-gutters': {
    backgroundColor: '#00000010',
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

globalThis.cm6_themes_solarizedLightTheme = solarizedLightTheme;

function solarizedLightHighlightStyle() {
  var _highlight = codemirror.Lezer_highlight;
  return [{
  tag: _highlight.tags.keyword,
  color: base_green
}, {
  tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.propertyName, _highlight.tags.macroName],
  color: base_cyan
}, {
  tag: [_highlight.tags.variableName],
  color: base_blue
}, {
  tag: [_highlight.tags["function"](_highlight.tags.variableName)],
  color: base_blue
}, {
  tag: [_highlight.tags.labelName],
  color: base_magenta
}, {
  tag: [_highlight.tags.color, _highlight.tags.constant(_highlight.tags.name), _highlight.tags.standard(_highlight.tags.name)],
  color: base_yellow
}, {
  tag: [_highlight.tags.definition(_highlight.tags.name), _highlight.tags.separator],
  color: base_cyan
}, {
  tag: [_highlight.tags.brace],
  color: base_magenta
}, {
  tag: [_highlight.tags.annotation],
  color: invalid
}, {
  tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
  color: base_magenta
}, {
  tag: [_highlight.tags.typeName, _highlight.tags.className],
  color: base_orange
}, {
  tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
  color: base_violet
}, {
  tag: [_highlight.tags.tagName],
  color: base_blue
}, {
  tag: [_highlight.tags.squareBracket],
  color: base_red
}, {
  tag: [_highlight.tags.angleBracket],
  color: base02
}, {
  tag: [_highlight.tags.attributeName],
  color: base1
}, {
  tag: [_highlight.tags.regexp],
  color: invalid
}, {
  tag: [_highlight.tags.quote],
  color: base_green
}, {
  tag: [_highlight.tags.string],
  color: base_yellow
}, {
  tag: _highlight.tags.link,
  color: base_cyan,
  textDecoration: 'underline',
  textUnderlinePosition: 'under'
}, {
  tag: [_highlight.tags.url, _highlight.tags.escape, _highlight.tags.special(_highlight.tags.string)],
  color: base_yellow
}, {
  tag: [_highlight.tags.meta],
  color: base_red
}, {
  tag: [_highlight.tags.comment],
  color: base02,
  fontStyle: 'italic'
}, {
  tag: _highlight.tags.strong,
  fontWeight: 'bold',
  color: base01
}, {
  tag: _highlight.tags.emphasis,
  fontStyle: 'italic',
  color: base_green
}, {
  tag: _highlight.tags.strikethrough,
  textDecoration: 'line-through'
}, {
  tag: _highlight.tags.heading,
  fontWeight: 'bold',
  color: base_yellow
}, {
  tag: _highlight.tags.heading1,
  fontWeight: 'bold',
  color: base03
}, {
  tag: [_highlight.tags.heading2, _highlight.tags.heading3, _highlight.tags.heading4],
  fontWeight: 'bold',
  color: base03
}, {
  tag: [_highlight.tags.heading5, _highlight.tags.heading6],
  color: base03
}, {
  tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
  color: base_magenta
}, {
  tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted, _highlight.tags.contentSeparator],
  color: base_red
}, {
  tag: [_highlight.tags.contentSeparator],
  color: base_yellow
}, {
  tag: _highlight.tags.invalid,
  color: base02,
  borderBottom: "1px dotted ".concat(base_red)
}]
}

globalThis.cm6_themes_solarizedLightHighlightStyle = solarizedLightHighlightStyle;

