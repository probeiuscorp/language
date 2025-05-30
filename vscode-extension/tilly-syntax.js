const fs = require('node:fs/promises');

const syntax = {
  $schema: 'https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json',
  name: 'Tilly',
  patterns: [
    { include: '#comments' },
    { include: '#strings' },
    { include: '#numbers' },
    { include: '#properties' },
    { include: '#declarations' },
    { include: '#keywords' },
    { include: '#operators' },
    { include: '#control' },
    { include: '#identifiers' },
  ],
  repository: {
    keywords: {
      patterns: [{
        name: 'keyword.other.tilly',
        match: '\\b(match|where)\\b',
      }, {
        match: '[\\w\\s](=|:)[\\w\\s]',
        captures: {
          1: {
            name: 'keyword.other.tilly',
          },
        },
      }, {
        name: 'keyword.other.tilly',
        match: '^(import|export\\s+type|export|type|data|export\\s+data|infix[lr]?)|instance|export\\s+class|class',
      }, {
        match: '\\S(\\.)\\s',
        captures: {
          1: {
            name: 'keyword.other.period.tilly',
          },
        },
      }, {
        match: '\\b(forall)\\b',
        name: 'keyword.other.forall.tilly',
      }],
    },
    properties: {
      match: '\\.\\w+',
      name: 'string.tilly.property',
    },
    operators: {
      // See https://github.com/JustusAdam/language-haskell/blob/298729dce888c1d5f32f545037d9d7276e2f1e57/syntaxes/haskell.YAML-tmLanguage#L600
      match: '(?x)((?:(?<!\'\')(\'\')?[\\p{Lu}\\p{Lt}][\\p{Ll}_\\p{Lu}\\p{Lt}\\p{Nd}\'\']*\\.)*)(\\#+|[\\p{S}\\p{P}&&[^(),;\\[\\]`{}_"\']]+(?<!\\#))',
      name: 'keyword.operator',
    },
    identifiers: {
      match: '\\b([A-Z]\\w*)\\b',
      name: 'constant.other.tilly',
    },
    declarations: {
      patterns: [{
        match: '^(import)\\s+(\\S+)(\\s+as|\\s+hiding)?',
        captures: {
          1: {
            name: 'keyword.other.tilly',
          },
          2: {
            name: 'string.tilly',
          },
          3: {
            name: 'keyword.other.tilly',
          },
        },
      }],
    },
    strings: {
      name: 'string.quoted.double.tilly',
      begin: '"',
      end: '"',
      patterns: [{
        name: 'constant.character.escape.tilly',
        match: '\\\\.',
      }],
    },
    numbers: {
      patterns: [{
        name: 'constant.numeric',
        match: '\\b[0-9]+(\\.[0-9]*)?\\b',
      }, {
        name: 'constant.numeric',
        match: '\\b0[xX][0-9a-fA-F]*(\\.[0-9a-fA-F]*)?\\b',
      }, {
        name: 'constant.numeric',
        match: '\\b0[bB][01]*(\\.[01]*)?\\b',
      }],
    },
    comments: {
      patterns: [{
        name: 'comment',
        begin: '\\/\\*',
        end: '\\*\\/',
      }, {
        name: 'comment',
        begin: '//',
        end: '\\n',
      }],
    },
  },
  scopeName: 'source.tilly',
};

fs.writeFile('syntaxes/tilly.tmLanguage.json', JSON.stringify(syntax, undefined, 2));
