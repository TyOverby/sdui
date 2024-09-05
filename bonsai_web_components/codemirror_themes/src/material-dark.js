var base00 = '#2e3235',
    base01 = '#505d64',
    base02 = '#606f7a',
    base03 = '#707d8b',
    base04 = '#a0a4ae',
    base05 = '#bdbdbd',
    base06 = '#e0e0e0',
    base07 = '#fdf6e3',
    base_red = '#ff5f52',
    base_deeporange = '#ff6e40',
    base_pink = '#fa5788',
    base_yellow = '#facf4e',
    base_orange = '#ffad42',
    base_cyan = '#56c8d8',
    base_indigo = '#7186f0',
    base_purple = '#cf6edf',
    base_green = '#6abf69',
    base_lightgreen = '#99d066',
    base_teal = '#4ebaaa';
var invalid = base_red,
    darkBackground = '#202325',
    highlightBackground = '#545b61',
    background = base00,
    tooltipBackground = base01,
    selection = base01,
    cursor = base04; /// The editor theme styles for Material Dark.

var materialDarkTheme = {
  '&': {
    color: base05,
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
    outline: "1px solid ".concat(base_yellow),
    backgroundColor: 'transparent'
  },
  '.cm-searchMatch.cm-searchMatch-selected': {
    backgroundColor: highlightBackground
  },
  '.cm-activeLine': {
    backgroundColor: highlightBackground
  },
  '.cm-selectionMatch': {
    backgroundColor: darkBackground,
    outline: "1px solid ".concat(base_teal)
  },
  '&.cm-focused .cm-matchingBracket': {
    color: base06,
    outline: "1px solid ".concat(base_teal)
  },
  '&.cm-focused .cm-nonmatchingBracket': {
    color: base_red
  },
  '.cm-gutters': {
    backgroundColor: base00,
    borderRight: '1px solid #4f5b66',
    color: base02
  },
  '.cm-activeLineGutter': {
    backgroundColor: highlightBackground,
    color: base07
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

globalThis.cm6_themes_materialDarkTheme = materialDarkTheme;

function materialDarkHighlightStyle(){
  var _highlight = codemirror.Lezer_highlight;
  return [{
  tag: _highlight.tags.keyword,
  color: base_purple
}, {
  tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.macroName],
  color: base_cyan
}, {
  tag: [_highlight.tags.propertyName],
  color: base_yellow
}, {
  tag: [_highlight.tags.variableName],
  color: base05
}, {
  tag: [_highlight.tags["function"](_highlight.tags.variableName)],
  color: base_cyan
}, {
  tag: [_highlight.tags.labelName],
  color: base_purple
}, {
  tag: [_highlight.tags.color, _highlight.tags.constant(_highlight.tags.name), _highlight.tags.standard(_highlight.tags.name)],
  color: base_yellow
}, {
  tag: [_highlight.tags.definition(_highlight.tags.name), _highlight.tags.separator],
  color: base_pink
}, {
  tag: [_highlight.tags.brace],
  color: base_purple
}, {
  tag: [_highlight.tags.annotation],
  color: invalid
}, {
  tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
  color: base_orange
}, {
  tag: [_highlight.tags.typeName, _highlight.tags.className],
  color: base_orange
}, {
  tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
  color: base_indigo
}, {
  tag: [_highlight.tags.tagName],
  color: base_deeporange
}, {
  tag: [_highlight.tags.squareBracket],
  color: base_red
}, {
  tag: [_highlight.tags.angleBracket],
  color: base02
}, {
  tag: [_highlight.tags.attributeName],
  color: base05
}, {
  tag: [_highlight.tags.regexp],
  color: invalid
}, {
  tag: [_highlight.tags.quote],
  color: base_green
}, {
  tag: [_highlight.tags.string],
  color: base_lightgreen
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
  color: base03
}, {
  tag: [_highlight.tags.comment],
  color: base03,
  fontStyle: 'italic'
}, {
  tag: _highlight.tags.monospace,
  color: base05
}, {
  tag: _highlight.tags.strong,
  fontWeight: 'bold',
  color: base_red
}, {
  tag: _highlight.tags.emphasis,
  fontStyle: 'italic',
  color: base_lightgreen
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
  color: base_yellow
}, {
  tag: [_highlight.tags.heading2, _highlight.tags.heading3, _highlight.tags.heading4],
  fontWeight: 'bold',
  color: base_yellow
}, {
  tag: [_highlight.tags.heading5, _highlight.tags.heading6],
  color: base_yellow
}, {
  tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
  color: base_cyan
}, {
  tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted],
  color: base_red
}, {
  tag: [_highlight.tags.contentSeparator],
  color: base_cyan
}, {
  tag: _highlight.tags.invalid,
  color: base02,
  borderBottom: "1px dotted ".concat(base_red)
}]
}

globalThis.cm6_themes_materialDarkHighlightStyle = materialDarkHighlightStyle;

