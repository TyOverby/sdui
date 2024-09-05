/* Editor UI colors */
var darkColors = {
    editorForeground: 'var(--vscode-editor-foreground, #CCCCCC)',
    editorBackground: 'var(--vscode-editor-background, #1F1F1F)',
    editorSelectionBackground: 'var(--vscode-editor-selectionBackground, #0080ff66)',
    editorLineNumberForeground: 'var(--vscode-editorLineNumber-foreground, #6E7681)',
    panelForeground: 'var(--vscode--panel--foreground, #CCCCCC)',
    panelBackground: 'var(--vscode--panel--background, #181818)',
    panelBorder: 'var(--vscode--panel--border, #2B2B2B)',
};
var lightColors = {
    editorForeground: 'var(--vscode-editor-foreground, #3B3B3B)',
    editorBackground: 'var(--vscode-editor-background, #FFFFFF)',
    editorSelectionBackground: 'var(--vscode-editor-selectionBackground, #0080ff66)',
    editorLineNumberForeground: 'var(--vscode-editorLineNumber-foreground, #6E7681)',
    panelForeground: 'var(--vscode--panel--foreground, #3B3B3B)',
    panelBackground: 'var(--vscode--panel--background, #F8F8F8)',
    panelBorder: 'var(--vscode--panel--border, #E5E5E5)',
};

/* Token colors */
var darkTokenColors = {
    tokenFunction: '#DCDCAA',
    tokenType: '#4EC9B0',
    tokenKeyword: '#569CD6',
    tokenPunctuation: darkColors.editorForeground,
    tokenParens: '#C586C0',
    tokenVarName: '#9CDCFE',
    tokenConstant: '#4FC1FF',
    tokenPropery: '#CE9178',
    tokenComment: '#6A9955',
    tokenString: '#CE9178',
    tokenInvalid: 'var(--vscode-errorForeground, #F85149)',
};
var lightTokenColors = {
    tokenFunction: '#795E26',
    tokenType: '#267f99',
    tokenKeyword: '#0000FF',
    tokenPunctuation: lightColors.editorForeground,
    tokenParens: '#AF00DB',
    tokenVarName: '#001080',
    tokenConstant: '#0070C1',
    tokenPropery: '#0451a5',
    tokenComment: '#008000',
    tokenString: '#A31515',
    tokenInvalid: 'var(--vscode-errorForeground, #F85149)',
};

function vscodeTheme({
    editorForeground,
    editorBackground,
    editorSelectionBackground,
    editorLineNumberForeground,
    panelBackground,
    panelForeground,
    panelBorder,
}) {
    return {
        '&': {
            color: editorForeground,
            backgroundColor: editorBackground,
        },
        '.cm-content': {
            caretColor: editorForeground
        },
        '&.cm-focused .cm-cursor, .cm-dropCursor': {
            borderLeftColor: editorForeground,
            borderLeftWidth: '2px',
        },
        '&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection': {
            color: 'unset',
            backgroundColor: editorSelectionBackground,
        },
        '.cm-panels': {
            backgroundColor: panelBackground,
            color: panelForeground,
        },
        '.cm-panels.cm-panels-top': {
            borderBottom: '1px solid '.concat(panelBorder),
        },
        '.cm-panels.cm-panels-bottom': {
            borderTop: '1px solid '.concat(panelBorder),
        },
        '.cm-searchMatch': {
            backgroundColor: editorSelectionBackground,
            outline: "1px solid ".concat(editorSelectionBackground),
        },
        '.cm-searchMatch.cm-searchMatch-selected': {
            backgroundColor: editorSelectionBackground,
        },
        '.cm-activeLine': {
            backgroundColor: editorSelectionBackground,
        },
        '.cm-selectionMatch': {
            backgroundColor: editorSelectionBackground,
        },
        '&.cm-focused .cm-matchingBracket, &.cm-focused .cm-nonmatchingBracket': {
            outline: "1px solid ".concat(editorSelectionBackground),
        },
        '&.cm-focused .cm-matchingBracket': {
            backgroundColor: editorSelectionBackground,
        },
        '.cm-gutters': {
            backgroundColor: editorBackground,
            color: editorLineNumberForeground,
            border: 'none',
            userSelect: 'none',
        },
        '.cm-activeLineGutter': {
            backgroundColor: panelBackground,
        },
        '.cm-foldPlaceholder': {
            backgroundColor: panelBackground,
            border: '1px solid '.concat(panelBorder),
            borderRadius: '4px',
            color: panelForeground,
        },
        '.cm-tooltip': {
            border: '1px solid '.concat(panelBorder),
            borderRadius: '4px',
            backgroundColor: panelBackground,
            color: panelForeground,
        },
        '.cm-tooltip .cm-tooltip-arrow:before': {
            display: 'none',
        },
        '.cm-tooltip .cm-tooltip-arrow:after': {
            display: 'none',
        },
        '.cm-tooltip-autocomplete': {
            '& > ul > li[aria-selected]': {
                backgroundColor: editorSelectionBackground,
                color: panelForeground,
            }
        }
    }
}

function vscodeHighlightStyle(
    {
        editorForeground,
    },
    {
        tokenFunction,
        tokenType,
        tokenKeyword,
        tokenPunctuation,
        tokenParens,
        tokenVarName,
        tokenConstant,
        tokenPropery,
        tokenComment,
        tokenString,
        tokenInvalid,
    }) {
    return function () {
        var _highlight = codemirror.Lezer_highlight;
        return [{
            tag: _highlight.tags.keyword,
            color: tokenKeyword
        }, {
            tag: [_highlight.tags.name, _highlight.tags.deleted, _highlight.tags.character, _highlight.tags.propertyName, _highlight.tags.macroName],
            color: tokenPropery
        }, {
            tag: [_highlight.tags.variableName],
            color: tokenVarName
        }, {
            tag: [_highlight.tags["function"](_highlight.tags.variableName)],
            color: tokenFunction
        }, {
            tag: [_highlight.tags.labelName],
            color: tokenConstant
        }, {
            tag: [_highlight.tags.color, _highlight.tags.constant(_highlight.tags.name), _highlight.tags.standard(_highlight.tags.name)],
            color: tokenPropery
        }, {
            tag: [_highlight.tags.definition(_highlight.tags.name), _highlight.tags.separator],
            color: tokenConstant
        }, {
            tag: [_highlight.tags.brace],
            color: tokenPunctuation
        }, {
            tag: [_highlight.tags.annotation],
            color: tokenInvalid
        }, {
            tag: [_highlight.tags.number, _highlight.tags.changed, _highlight.tags.annotation, _highlight.tags.modifier, _highlight.tags.self, _highlight.tags.namespace],
            color: tokenConstant
        }, {
            tag: [_highlight.tags.typeName, _highlight.tags.className],
            color: tokenType
        }, {
            tag: [_highlight.tags.operator, _highlight.tags.operatorKeyword],
            color: tokenPunctuation
        }, {
            tag: [_highlight.tags.tagName],
            color: tokenPropery
        }, {
            tag: [_highlight.tags.squareBracket],
            color: tokenParens
        }, {
            tag: [_highlight.tags.angleBracket],
            color: tokenParens
        }, {
            tag: [_highlight.tags.attributeName],
            color: tokenVarName
        }, {
            tag: [_highlight.tags.regexp],
            color: tokenParens
        }, {
            tag: [_highlight.tags.quote],
            color: tokenType
        }, {
            tag: [_highlight.tags.string],
            color: tokenString
        }, {
            tag: _highlight.tags.link,
            color: tokenComment
        }, {
            tag: [_highlight.tags.url, _highlight.tags.escape, _highlight.tags.special(_highlight.tags.string)],
            color: tokenFunction,
            textDecoration: 'underline',
            textUnderlinePosition: 'under'
        }, {
            tag: [_highlight.tags.meta],
            color: tokenType
        }, {
            tag: [_highlight.tags.comment],
            color: tokenComment,
            fontStyle: 'italic'
        }, {
            tag: _highlight.tags.strong,
            fontWeight: 'bold',
            color: tokenVarName
        }, {
            tag: _highlight.tags.emphasis,
            fontStyle: 'italic',
            color: tokenVarName
        }, {
            tag: _highlight.tags.strikethrough,
            textDecoration: 'line-through'
        }, {
            tag: _highlight.tags.heading,
            fontWeight: 'bold',
            color: editorForeground
        }, {
            tag: _highlight.tags.special(_highlight.tags.heading1),
            fontWeight: 'bold',
            color: tokenConstant
        }, {
            tag: _highlight.tags.heading1,
            fontWeight: 'bold',
            color: tokenConstant
        }, {
            tag: [_highlight.tags.heading2, _highlight.tags.heading3, _highlight.tags.heading4],
            fontWeight: 'bold',
            color: tokenType
        }, {
            tag: [_highlight.tags.heading5, _highlight.tags.heading6],
            color: tokenType
        }, {
            tag: [_highlight.tags.atom, _highlight.tags.bool, _highlight.tags.special(_highlight.tags.variableName)],
            color: tokenFunction
        }, {
            tag: [_highlight.tags.processingInstruction, _highlight.tags.inserted],
            color: tokenFunction
        }, {
            tag: [_highlight.tags.contentSeparator],
            color: tokenPunctuation
        }, {
            tag: _highlight.tags.invalid,
            color: editorForeground,
            borderBottom: "1px dotted ".concat(tokenInvalid)
        }]
    };
}

/* Expose the dark and light themes to JS. */
globalThis.cm6_themes_vscodeDarkTheme = vscodeTheme(darkColors);
globalThis.cm6_themes_vscodeDarkHighlightStyle = vscodeHighlightStyle(darkColors, darkTokenColors);
globalThis.cm6_themes_vscodeLightTheme = vscodeTheme(lightColors);
globalThis.cm6_themes_vscodeLightHighlightStyle = vscodeHighlightStyle(lightColors, lightTokenColors);
