---@meta

---@class (exact) Node
---@field kind string

---@class Expression: Node

---@class (exact) GroupExpression: Expression
---@field kind 'GroupExpression'
---@field expression Expression

---@class (exact) Identifier: Expression
---@field kind 'Identifier'
---@field value string

---@alias literal string|number|boolean|nil

---@class (exact) Literal: Expression
---@field kind 'Literal'
---@field value literal

---@class (exact) RecordExpression: Expression
---@field kind 'RecordExpression'
---@field entries Entry[]
---@field destructure? boolean

---@class (exact) Entry: Node
---@field kind 'Entry'
---@field field? Literal
---@field property Expression

---@class (exact) CallExpression: Expression
---@field kind 'CallExpression'
---@field callee Expression
---@field arguments Expression[]

---@class (exact) MemberExpression: Expression
---@field kind 'MemberExpression'
---@field id Expression
---@field record Expression
---@field computed boolean
---@field indexer? string

---@class (exact) BlockStatement: Statement
---@field kind 'BlockStatement'
---@field body Statement[]

---@alias Parameter Identifier|Literal

---@class (exact) FunctionExpression: Expression
---@field kind 'FunctionExpression'
---@field parameters Parameter[]
---@field body BlockStatement

---@class (exact) UnaryExpression: Expression
---@field kind 'UnaryExpression'
---@field operator string
---@field argument Expression

---@class (exact) BinaryExpression: Expression
---@field kind 'BinaryExpression'
---@field operator string
---@field left Expression
---@field right Expression

---@class (exact) ExpressionStatement: Statement
---@field kind 'ExpressionStatement'
---@field expression Expression

---@class (exact) UpdateExpression: UnaryExpression
---@field kind 'UpdateExpression'

---@class (exact) AssignmentExpression: BinaryExpression
---@field kind 'AssignmentExpression'

---@class Statement: Node

---@class (exact) VariableDeclaration: Statement
---@field kind 'VariableDeclaration'
---@field declarations VariableDeclarator[]

---@class (exact) VariableDeclarator: Node
---@field kind 'VariableDeclarator'
---@field id Identifier
---@field init? Expression

---@class (exact) FunctionDeclaration: Statement
---@field kind 'FunctionDeclaration'
---@field name Identifier
---@field parameters Parameter[]
---@field body BlockStatement

---@class (exact) ReturnStatement: Statement
---@field kind 'ReturnStatement'
---@field arguments Expression[]

---@class (exact) IfStatement: Statement
---@field kind 'IfStatement'
---@field test Expression
---@field consequent BlockStatement
---@field alternate? BlockStatement
---@field [string] Node

---@class (exact) DoStatement: Statement
---@field kind 'DoStatement'
---@field body BlockStatement
---@field loop? string
---@field test? Expression

---@class (exact) ForStatement: DoStatement
---@field kind 'ForStatement'
---@field init AssignmentExpression
---@field goal Expression
---@field step? Expression

---@class (exact) ForEachStatement: DoStatement
---@field kind 'ForEachStatement'
---@field left Identifier[]
---@field operator string
---@field right Expression

---@class (exact) BreakStatement: Statement
---@field kind 'BreakStatement'

---@class (exact) Program
---@field kind 'Program'
---@field body Statement[]