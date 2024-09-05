var dark0 = '#282828',
    dark1 = '#3c3836',
    dark2 = '#504945',
    dark3 = '#665c54',
    dark4 = '#7c6f64',
    dark4_256 = '#7c6f64',
    gray_245 = '#928374',
    gray_244 = '#928374',
    light0 = '#fbf1c7',
    light1 = '#ebdbb2',
    light2 = '#d5c4a1',
    light3 = '#bdae93',
    light4 = '#a89984',
    light4_256 = '#a89984',
    bright_red = '#fb4934',
    bright_green = '#b8bb26',
    bright_yellow = '#fabd2f',
    bright_blue = '#83a598',
    bright_purple = '#d3869b',
    bright_aqua = '#8ec07c',
    bright_orange = '#fe8019',
    neutral_red = '#cc241d',
    neutral_green = '#98971a',
    neutral_yellow = '#d79921',
    neutral_blue = '#458588',
    neutral_purple = '#b16286',
    neutral_aqua = '#689d6a',
    neutral_orange = '#d65d0e',
    faded_red = '#9d0006',
    faded_green = '#79740e',
    faded_yellow = '#b57614',
    faded_blue = '#076678',
    faded_purple = '#8f3f71',
    faded_aqua = '#427b58',
    faded_orange = '#af3a03';
var bg0 = dark0,
    bg1 = dark1,
    bg2 = dark2,
    bg3 = dark3,
    bg4 = dark4,
    gray = gray_245,
    fg0 = light0,
    fg1 = light1,
    fg2 = light2,
    fg3 = light3,
    fg4 = light4,
    fg4_256 = light4_256,
    red = bright_red,
    green = bright_green,
    yellow = bright_yellow,
    blue = bright_blue,
    purple = bright_purple,
    aqua = bright_aqua,
    orange = bright_orange;
var invalid = red,
    darkBackground = bg1,
    highlightBackground = darkBackground,
    background = bg0,
    tooltipBackground = bg1,
    selection = darkBackground,
    cursor = orange; /// The editor theme styles for Gruvbox Dark.

var gruvboxDarkTheme = {
  '&': {
    color: fg1,
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
    color: fg1
  },
  '.cm-panels.cm-panels-top': {
    borderBottom: '2px solid black'
  },
  '.cm-panels.cm-panels-bottom': {
    borderTop: '2px solid black'
  },
  '.cm-searchMatch': {
    backgroundColor: bg0,
    color: yellow,
    outline: "1px solid ".concat(bg3)
  },
  '.cm-searchMatch.cm-searchMatch-selected': {
    backgroundColor: bg3
  },
  '.cm-activeLine': {
    backgroundColor: highlightBackground
  },
  '.cm-selectionMatch': {
    backgroundColor: bg3
  },
  '&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket': {
    outline: "1px solid ".concat(bg3),
    fontStyle: 'bold'
  },
  '&.cm-focused .cm-matchingBracket': {
    backgroundColor: bg3
  },
  '.cm-gutters': {
    backgroundColor: bg1,
    color: fg3,
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
      color: fg2
    }
  }
}

globalThis.cm6_themes_gruvboxDarkTheme = gruvboxDarkTheme;

function gruvboxDarkHighlightStyle(){
  var _highlight = codemirror.Lezer_highlight;
  return [{
  tag: _highlight.tags.keyword,
  color: red
}, {
  tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.propertyName, _highlight.tags.macroName],
  color: aqua
}, {
  tag: [_highlight.tags.variableName],
  color: blue
}, {
  tag: [_highlight.tags["function"](_highlight.tags.variableName)],
  color: green,
  fontStyle: 'bold'
}, {
  tag: [_highlight.tags.labelName],
  color: fg1
}, {
  tag: [_highlight.tags.color, _highlight.tags.constant(_highlight.tags.name), _highlight.tags.standard(_highlight.tags.name)],
  color: purple
}, {
  tag: [_highlight.tags.definition(_highlight.tags.name), _highlight.tags.separator],
  color: fg1
}, {
  tag: [_highlight.tags.brace],
  color: fg1
}, {
  tag: [_highlight.tags.annotation],
  color: invalid
}, {
  tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
  color: purple
}, {
  tag: [_highlight.tags.typeName, _highlight.tags.className],
  color: yellow
}, {
  tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
  color: red
}, {
  tag: [_highlight.tags.tagName],
  color: aqua,
  fontStyle: 'bold'
}, {
  tag: [_highlight.tags.squareBracket],
  color: orange
}, {
  tag: [_highlight.tags.angleBracket],
  color: blue
}, {
  tag: [_highlight.tags.attributeName],
  color: aqua
}, {
  tag: [_highlight.tags.regexp],
  color: aqua
}, {
  tag: [_highlight.tags.quote],
  color: gray
}, {
  tag: [_highlight.tags.string],
  color: fg1
}, {
  tag: _highlight.tags.link,
  color: fg4,
  textDecoration: 'underline',
  textUnderlinePosition: 'under'
}, {
  tag: [_highlight.tags.url, _highlight.tags.escape, _highlight.tags.special(_highlight.tags.string)],
  color: purple
}, {
  tag: [_highlight.tags.meta],
  color: yellow
}, {
  tag: [_highlight.tags.comment],
  color: gray,
  fontStyle: 'italic'
}, {
  tag: _highlight.tags.strong,
  fontWeight: 'bold',
  color: orange
}, {
  tag: _highlight.tags.emphasis,
  fontStyle: 'italic',
  color: green
}, {
  tag: _highlight.tags.strikethrough,
  textDecoration: 'line-through'
}, {
  tag: _highlight.tags.heading,
  fontWeight: 'bold',
  color: green
}, {
  tag: [_highlight.tags.heading1, _highlight.tags.heading2],
  fontWeight: 'bold',
  color: green
}, {
  tag: [_highlight.tags.heading3, _highlight.tags.heading4],
  fontWeight: 'bold',
  color: yellow
}, {
  tag: [_highlight.tags.heading5, _highlight.tags.heading6],
  color: yellow
}, {
  tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
  color: purple
}, {
  tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted],
  color: bright_blue
}, {
  tag: [_highlight.tags.contentSeparator],
  color: red
}, {
  tag: _highlight.tags.invalid,
  color: orange,
  borderBottom: "1px dotted ".concat(invalid)
}]
}


globalThis.cm6_themes_gruvboxDarkHighlightStyle = gruvboxDarkHighlightStyle;

