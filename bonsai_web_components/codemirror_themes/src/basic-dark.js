var base00 = '#2E3235',
    base01 = '#DDDDDD',
    base02 = '#B9D2FF',
    base03 = '#b0b0b0',
    base04 = '#d0d0d0',
    base05 = '#e0e0e0',
    base06 = '#808080',
    base07 = '#000000',
    base08 = '#A54543',
    base09 = '#fc6d24',
    base0A = '#fda331',
    base0B = '#8abeb7',
    base0C = '#b5bd68',
    base0D = '#6fb3d2',
    base0E = '#cc99cc',
    base0F = '#6987AF';
var invalid = base09,
    darkBackground = '#292d30',
    highlightBackground = base02 + '30',
    background = base00,
    tooltipBackground = base01,
    selection = '#202325',
    cursor = base01; /// The editor theme styles for Basic Dark.

var basicDarkTheme = {
  '&': {
    color: base01,
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
    backgroundColor: base02,
    outline: "1px solid ".concat(base03),
    color: base07
  },
  '.cm-searchMatch.cm-searchMatch-selected': {
    backgroundColor: base05,
    color: base07
  },
  '.cm-activeLine': {
    backgroundColor: highlightBackground
  },
  '.cm-selectionMatch': {
    backgroundColor: highlightBackground
  },
  '&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket': {
    outline: "1px solid ".concat(base03)
  },
  '&.cm-focused .cm-matchingBracket': {
    backgroundColor: base02,
    color: base07
  },
  '.cm-gutters': {
    borderRight: "1px solid #ffffff10",
    color: base06,
    backgroundColor: darkBackground
  },
  '.cm-activeLineGutter': {
    backgroundColor: highlightBackground
  },
  '.cm-foldPlaceholder': {
    backgroundColor: 'transparent',
    border: 'none',
    color: base02
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

globalThis.cm6_themes_basicDarkTheme = basicDarkTheme;


function basicDarkHighlightStyle() {
  var _highlight = codemirror.Lezer_highlight;

return [{
  tag: _highlight.tags.keyword,
  color: base0A
}, {
  tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.propertyName, _highlight.tags.macroName],
  color: base0C
}, {
  tag: [_highlight.tags.variableName],
  color: base0D
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
  color: base0E
}, {
  tag: [_highlight.tags.annotation],
  color: invalid
}, {
  tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
  color: base0A
}, {
  tag: [_highlight.tags.typeName, _highlight.tags.className],
  color: base0D
}, {
  tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
  color: base0E
}, {
  tag: [_highlight.tags.tagName],
  color: base0A
}, {
  tag: [_highlight.tags.squareBracket],
  color: base0E
}, {
  tag: [_highlight.tags.angleBracket],
  color: base0E
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
  color: base0F,
  textDecoration: 'underline',
  textUnderlinePosition: 'under'
}, {
  tag: [_highlight.tags.url, _highlight.tags.escape, _highlight.tags.special(_highlight.tags.string)],
  color: base0B
}, {
  tag: [_highlight.tags.meta],
  color: base08
}, {
  tag: [_highlight.tags.comment],
  color: base06,
  fontStyle: 'italic'
}, {
  tag: _highlight.tags.monospace,
  color: base01
}, {
  tag: _highlight.tags.strong,
  fontWeight: 'bold',
  color: base0A
}, {
  tag: _highlight.tags.emphasis,
  fontStyle: 'italic',
  color: base0D
}, {
  tag: _highlight.tags.strikethrough,
  textDecoration: 'line-through'
}, {
  tag: _highlight.tags.heading,
  fontWeight: 'bold',
  color: base01
}, {
  tag: _highlight.tags.special(_highlight.tags.heading1),
  fontWeight: 'bold',
  color: base01
}, {
  tag: _highlight.tags.heading1,
  fontWeight: 'bold',
  color: base01
}, {
  tag: [_highlight.tags.heading2, _highlight.tags.heading3, _highlight.tags.heading4],
  fontWeight: 'bold',
  color: base01
}, {
  tag: [_highlight.tags.heading5, _highlight.tags.heading6],
  color: base01
}, {
  tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
  color: base0B
}, {
  tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted],
  color: base0B
}, {
  tag: [_highlight.tags.contentSeparator],
  color: base0D
}, {
  tag: _highlight.tags.invalid,
  color: base02,
  borderBottom: "1px dotted ".concat(invalid)
}]
}

globalThis.cm6_themes_basicDarkHighlightStyle = basicDarkHighlightStyle;

