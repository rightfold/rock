'use strict';

var escodegen = require('escodegen');

exports.callExpression = function(callee) {
    return function(arguments_) {
        return {
            type: 'CallExpression',
            callee: callee,
            arguments: arguments_,
        };
    };
};

exports.functionExpression = function(params) {
    return function(body) {
        return {
            type: 'FunctionExpression',
            id: null,
            params: params,
            defaults: [],
            rest: null,
            body: {
                type: 'BlockStatement',
                body: body,
            },
            generator: false,
            expression: false,
        };
    };
};

exports.returnStatement = function(argument) {
    return {
        type: 'ReturnStatement',
        argument: argument,
    };
};

exports.identifier = function(name) {
    return {
        type: 'Identifier',
        name: name,
    };
};

function literal(value) {
    return {
        type: 'Literal',
        value: value,
    };
};

exports.booleanLiteral = literal;
exports.intLiteral     = literal;
exports.numberLiteral  = literal;
exports.stringLiteral  = literal;
exports.nullLiteral    = literal(null);

function pretty(node) {
    return escodegen.generate(node);
};

exports.prettyExpression = pretty;
