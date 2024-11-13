local json = require 'json'

---@type string, string
local sourcePath, sourceContent
---@type integer?, string?, integer, integer
local lastIndex, lastValue, currentIndex, currentRow
local parseExpression, parseStatement

---@generic V: any
---@param a V
---@param ... any 
---@return V?
local function match (a, ...)
	for b = 1, select('#', ...) do
		if a == select(b, ...) then
			return a
		end
	end
end

---@return boolean
local function eof ()
	return not string.byte(sourceContent, currentIndex)
end

---@param p string
---@param ... string
---@return string?
local function parse (p, ...)
	---@type integer?, string
	local toIndex, firstValue
	---@type integer?, integer?, string, string
	lastIndex, toIndex, firstValue, lastValue = string.find(sourceContent, '^(%s*)' .. p, currentIndex)
	if select('#', ...) == 0 and lastValue or match(lastValue, ...) then
		local _, rowCount = string.gsub(firstValue, '[\n\r]', '')
		currentIndex, currentRow = toIndex + 1, currentRow + rowCount
		return lastValue
	end
end

---@param m string
---@param from? integer
---@param pattern? string
local function throw (m, from, pattern)
	pattern, currentIndex = pattern or '(%S+)', from or lastIndex or currentIndex
	local hint = parse(pattern) and string.format("'%s'", lastValue) or '<eof>'
	--io.write(json.encode({ path = sourcePath, row = currentRow, message = (m .. ' near ' .. hint) }), '\n')
	io.write('[', sourcePath, '] smc:', currentRow, ' ', m, ' near ', hint, '.\n')
	os.exit()
end

---@type table<string, true>
local reservedKeywords = {
	['true'] = true, ['false'] = true, ['undef'] = true,
	['and'] = true, ['or'] = true, ['is'] = true,
	['not'] = true, ['sizeof'] = true, ['typeof'] = true,
	['var'] = true, ['def'] = true, ['return'] = true,
	['if'] = true, ['then'] = true, ['else'] = true,
	['do'] = true, ['while'] = true, ['until'] = true,
	['for'] = true, ['to'] = true, ['step'] = true, ['of'] = true, ['in'] = true,
	['break'] = true, ['end'] = true
}

---@type table<integer, string>
local escapedCharacters = {
	[34] = '\"', [36] = '$', [39] = '\'',
	[92] = '\\', [97] = 'a', [98] = 'b',
	[102] = 'f', [110] = 'n', [114] = 'r',
	[115] = 't', [118] = 'v'
}

local function parseIdentifier ()
	if parse('(%p)', '(') then
		local expression = parseExpression()
		if not parse('(%p)', ')') then
			throw("')' expected")
		end
		return ({ kind = 'GroupExpression', expression = expression } --[=[@as GroupExpression]=])
	elseif not parse('([_%a][_%w]*)') or reservedKeywords[lastValue] then
		throw('unexpected symbol')
	end
	return ({ kind = 'Identifier', value = lastValue } --[=[@as Identifier]=])
end

---@param str string
local function parseTemplate (str)
	local newStr = string.gsub(str, '${(.-)}', function (t)
		return string.format('" .. tostring(%s) .. "', t)
	end)
	return string.format('"%s"', newStr)
end

local function parseLiteral ()
	---@type literal
	local value
	repeat
		local startIndex = currentIndex
		-- string
		if parse("'([^']*)") then
			value = string.format("'%s'", lastValue):gsub('\\u(%x+)', function (c)
				local n = tonumber(c, 16)
				return '\\' .. (escapedCharacters[n] or string.char(n))
			end)
			if not parse('(%p)', "'") then
				throw('malformed string', startIndex, '([^\n\r]*)')
			end
			break
		elseif parse('"([^"]*)') then
			value = parseTemplate(lastValue):gsub('\\u(%x+)', function (c)
				local n = tonumber(c, 16)
				return '\\' .. (escapedCharacters[n] or string.char(n))
			end)
			if not parse('(%p)', '"') then
				throw('malformed string', startIndex, '([^\n\r]*)')
			end
			break
		-- number
		elseif parse('(%d[%d_]*)') then
			local firstValue, secondValue = lastValue, parse('(%.?%d[_%d]*)') or ''
			if parse('(%p)', '.') then
				throw('malformed number', startIndex)
			end
			value = string.gsub(firstValue .. secondValue, '_', '') + 0
			break
		elseif parse('(&)%f[%x]') then
			if not parse('(%x+)') then
				throw('malformed number', startIndex)
			end
			value = tonumber(lastValue, 16)
			break
		--- boolean/undef
		elseif parse('(%l+)', 'true', 'false', 'undef') then
			if lastValue == 'true' then
				value = true
			elseif lastValue == 'false' then
				value = false
			end
			break
		--- ellipsis
		elseif parse('(%p+)%f[^.]', '...') then
			value = lastValue
			break
		end
		return parseIdentifier()
	until true
	return ({ kind = 'Literal', value = value } --[=[@as Literal]=])
end

---@param dstrc? boolean
local function parseRecordExpression (dstrc)
	if parse('(%p)', '[') then
		---@type Entry[]
		local entries = {}
		if not parse('(%p)', ']') then
			repeat
				---@type Literal?
				local field
				local property = (parseExpression() --[=[@as Expression]=])
				if parse('(%p)', ':', ',') == ':' then
					if property.kind ~= 'Literal' or not match(type((property --[=[@as Literal]=]).value), 'string', 'number') then
						throw('<string> or <number> expected')
					elseif dstrc then
						throw('unexpected symbol')
					end
					field, property = property --[=[@as Literal]=], parseExpression()
					parse('(%p)', ',')
				end
				entries[#entries + 1] = ({ kind = 'Entry', field = field, property = property } --[=[@as Entry]=])
			until parse('(%p)', ']') or eof()
			if not lastValue then
				throw("']' expected")
			end
		end
		return ({ kind = 'RecordExpression', entries = entries, destructure = dstrc } --[=[@as RecordExpression]=])
	end
	return parseLiteral()
end

---@generic C
---@param callee C
local function parseCallExpression (callee)
	while parse('(%p)', '(') do
		---@type Expression[]
		local arguments = {}
		if not parse('(%p)', ')') then
			repeat
				arguments[#arguments + 1] = parseExpression()
			until not parse('(%p)', ',') or eof()
			if not parse('(%p)', ')') then
				throw("')' expected")
			end
		end
		callee = ({ kind = 'CallExpression', callee = callee, arguments = arguments } --[=[@as CallExpression]=])
	end
	return callee
end

---@param dstrc? boolean
local function parseMemberExpression (dstrc)
	---@type MemberExpression|CallExpression|RecordExpression|Literal|GroupExpression|Identifier
	local lastNode = parseRecordExpression(dstrc)
	if lastNode.kind == 'Identifier' then
		lastNode = parseCallExpression(lastNode)
		while parse('(%p)%f[#%P]', '.', ':', '[') do
			---@type string, Expression, boolean
			local indexer, id, computed
			if lastValue == '[' then
				id, computed = parseExpression(), true
				if not parse('(%p)', ']') then
					throw("']' expected")
				end
			else
				indexer, id, computed = lastValue, parseIdentifier(), false
				if id.kind ~= 'Identifier' then
					throw('<identifier> expected')
				end
			end
			lastNode = parseCallExpression({ kind = 'MemberExpression', record = lastNode, id = id, computed = computed, indexer = indexer } --[=[@as MemberExpression]=])
			if lastNode.kind == 'MemberExpression' and (lastNode --[=[@as MemberExpression]=]).indexer == ':' then
				throw('function arguments expected')
			end
		end
	end
	return lastNode
end

local function parseBlockStatement ()
	---@type Statement[]
	local body = {}
	while not parse('(%l+)', 'end') and not eof() do
		body[#body + 1] = parseStatement()
	end
	if not lastValue then
		throw("'end' expected")
	end
	return ({ kind = 'BlockStatement', body = body } --[=[@as BlockStatement]=])
end

local function parseFunctionExpression ()
	if parse('(%l+)', 'def') then
		---@type Parameter[]
		local parameters = {}
		if not parse('(%p)', '(') then
			throw("'(' expected")
		elseif not parse('(%p)', ')') then
			repeat
				local parameter = parseIdentifier()
				if parameter.kind ~= 'Identifier' then
					throw('<identifier> expected')
				end
				parameters[#parameters + 1] = (parameter --[=[@as Identifier]=])
			until not parse('(%p)', ',') or eof()
			if not parse('(%p)', ')') then
				throw("')' expected")
			end
		end
		return ({ kind = 'FunctionExpression', parameters = parameters, body = parseBlockStatement() } --[=[@as FunctionExpression]=])
	end
	return parseMemberExpression()
end

local function parseUnaryExpression ()
	while parse('(%l+)', 'not', 'sizeof', 'typeof') or parse('(%p+)', '#', '...', '-') do
		return ({ kind = 'UnaryExpression', operator = lastValue, argument = parseFunctionExpression() } --[=[@as UnaryExpression]=])
	end
	return parseFunctionExpression()
end

local function parseMultiplicativeExpression ()
	---@type Identifier|GroupExpression|Literal|RecordExpression|MemberExpression|CallExpression|FunctionExpression|UnaryExpression|BinaryExpression
	local left= parseUnaryExpression()
	while parse('(%p+)', '*', '/', '^', '%', '..') do
		left = ({ kind = 'BinaryExpression', left = left, operator = lastValue --[=[@as string]=], right = parseUnaryExpression() } --[=[@as BinaryExpression]=])
	end
	return left
end

local function parseAdditiveExpression ()
	local left = parseMultiplicativeExpression()
	while parse('(%p)', '+', '-') do
		left = ({ kind = 'BinaryExpression', left = left, operator = lastValue, right = parseMultiplicativeExpression() --[=[@as BinaryExpression]=]})
	end
	return left
end

local function parseComparisonExpression ()
	local left = parseAdditiveExpression()
	while parse('(%p+)', '==', '<>', '<', '<=', '>', '>=') do
		left = ({ kind = 'BinaryExpression', left = left, operator = lastValue, right = parseAdditiveExpression() } --[=[@as BinaryExpression]=])
	end
	return left
end

local function parseLogicalExpression ()
	local left = parseComparisonExpression()
	while parse('(%l+)', 'and', 'or') do
		left = ({ kind = 'BinaryExpression', left = left, operator = lastValue, right = parseComparisonExpression() } --[=[@as BinaryExpression]=])
	end
	return left
end

function parseExpression ()
	return parseLogicalExpression()
end

local function parseExpressionStatement ()
	---@type Expression
	local expression = parseMemberExpression(true)
	if not match(expression.kind, 'MemberExpression', 'CallExpression', 'RecordExpression', 'GroupExpression', 'Identifier') then
		throw('syntax error')
	elseif expression.kind == 'GroupExpression' then
		expression = parseCallExpression(expression)
	elseif expression.kind ~= 'CallExpression' then
		--- UpdateExpression
		if parse('(%p+)%f[^+-]', '++', '--') then
			expression = ({ kind = 'UpdateExpression', argument = expression, operator = lastValue } --[=[@as UpdateExpression]=])
		else
			--- AssignmentExpression
			---@type Expression
			local right
			local operator = parse("(%p+)%f[=]", "+", "-", "*", "/", "//", "^", "%", "..")
			if (expression.kind == 'RecordExpression' and operator) or not parse('(%p)', '=') then
				throw("'=' expected")
			end
			operator, right = operator or lastValue, parseExpression()
			if expression.kind == right.kind then
				right.destructure = true
			end
			expression = ({ kind = 'AssignmentExpression', left = expression, operator = operator, right = right } --[=[@as AssignmentExpression]=])
		end
	end
	parse('(%p)', ';')
	return ({ kind = 'ExpressionStatement', expression = expression } --[=[@as ExpressionStatement]=])
end

function parseStatement ()
	--- Comments
	if parse('(%p+)', '//', '/*') == '//' then
		repeat
			parse('([^\n\r]*)')
		until not parse('(%p+)', '//') or eof()
		return nil;
	elseif lastValue == '/*' then

	--- Statement
	elseif parse('(%l+)', 'var', 'def', 'return', 'if', 'do', 'while', 'for', 'break') then
		---@type VariableDeclaration|FunctionDeclaration|ReturnStatement|IfStatement|DoStatement|ForStatement|ForEachStatement|BreakStatement
		local node
		repeat
			--- VariableDeclaration
			if lastValue == 'var' then
				---@type VariableDeclarator[]
				local declarations = {}
				repeat
					---@type Expression?
					local init
					---@type Identifier|RecordExpression
					local id = parseRecordExpression(true)
					if not match(id.kind, 'RecordExpression', 'Identifier') then
						throw('<identifier> expected')
					elseif parse('(%p)', '=') then
						init = parseExpression()
					end
					declarations[#declarations + 1] = ({ kind = 'VariableDeclarator', id = id, init = init } --[=[@as VariableDeclarator]=])
				until not parse('(%p)', ',') or eof()
				node = ({ kind = 'VariableDeclaration', declarations = declarations } --[=[@as VariableDeclaration]=])
				break
			--- FunctionDeclaration
			elseif lastValue == 'def' then
				---@type Identifier, Parameter[]
				local name, parameters = parseIdentifier(), {}
				if name.kind ~= 'Identifier' then
					throw('<name> expected')
				elseif not parse('(%p)', '(') then
					throw("'(' expected")
				elseif not parse('(%p)', ')') then
					repeat
						local parameter = parseLiteral()
						if parameter.kind == 'GroupExpression' then
							throw("<identifier> or '...' expected")
						elseif parameters[#parameters] and (parameters[#parameters] --[=[@as Literal]=]).value == '...' and parameter.value == '...' then
							throw("')' expected")
						end
						parameters[#parameters + 1] = parameter
					until not parse('(%p)', ',') or eof()
					if not parse('(%p)', ')') then
						throw("')' expected")
					end
				end
				node = ({ kind = 'FunctionDeclaration', name = name, parameters = parameters, body = parseBlockStatement() } --[=[@as FunctionDeclaration]=])
				break
			--- ReturnStatement
			elseif lastValue == 'return' then
				---@type Expression[]
				local arguments = {}
				repeat
					arguments[#arguments + 1] = parseExpression()
				until not parse('(%p)', ',') or eof()
				node = ({ kind = 'ReturnStatement', arguments = arguments } --[=[@as ReturnStatement]=])
				break
			--- IfStatement
			elseif lastValue == 'if' then
				---@type table<string, Node>
				local finalNode = {}
				local currentNode = finalNode
				while lastValue == 'if' or lastValue == 'else' do
					---@type Expression
					local testExpression
					local currentValue, blockStatement = lastValue, { kind = 'BlockStatement', body = {} } --[=[@as BlockStatement]=]
					local isNested, targetKey = false, 'consequent'
					if currentValue == 'else' then
						isNested, targetKey = parse('(%l+)', 'if') --[=[@as boolean]=], 'alternate'
					end
					if lastValue == 'if' then
						testExpression = parseExpression()
						if not parse('(%l+)', 'then') then
							throw("'then' expected")
						end
					end
					while not parse('(%l+)', 'end') and not eof() do
						blockStatement.body[#blockStatement.body + 1] = parseStatement()
						if parse('(%l+)', 'else') then
							if currentValue == lastValue and not isNested then
								throw("'end' expected")
							end
							break
						end
					end
					if not lastValue then
						throw("'end' expected")
					end
					currentNode[targetKey] = isNested and ({ kind = 'IfStatement', test = testExpression, consequent = blockStatement } --[=[@as IfStatement]=]) or blockStatement
					currentNode = (targetKey == 'alternate') and currentNode[targetKey] or currentNode
					---@diagnostic disable-next-line
					currentNode.kind, currentNode.test = testExpression and 'IfStatement' or 'BlockStatement', testExpression
				end
				node = (finalNode --[=[@as IfStatement]=])
				break
			--- DoStatement
			elseif lastValue == 'do' or lastValue == 'while' then
				---@type string, Statement[]
				local keyword, body = lastValue, {}
				---@type string?, Expression?
				local loop, test
				if keyword == 'while' then
					loop, test = keyword, parseExpression()
					if not parse('(%l+)', 'do') then
						throw("'do' expected")
					end
				end
				while not parse('(%l+)', 'until', 'end') and not eof() do
					body[#body + 1] = parseStatement()
				end
				if not lastValue or (keyword == 'while' and lastValue == 'until')  then
					throw("'end' expected")
				elseif lastValue == 'until' then
					loop, test = lastValue, parseExpression()
				end
				node = ({ kind = 'DoStatement', test = test, loop = loop, body = { kind = 'BlockStatement', body = body } } --[=[@as DoStatement]=])
				break
			--- ForStatement
			elseif lastValue == 'for' then
				---@type ForStatement|ForEachStatement
				local finalNode
				local firstExpression = parseIdentifier()
				repeat
					if firstExpression.kind ~= 'Identifier' then
						throw('<variable> expected')
					elseif parse('(%p)', ',', '=') == '=' then
						---@type AssignmentExpression
						local init = { kind = 'AssignmentExpression', left = firstExpression, operator = lastValue, right = parseExpression() }
						if not parse('(%l+)', 'to') then
							throw("'to' expected")
						end
						---@type Expression, Expression?
						local goal, step = parseExpression(), parse('(%l+)', 'step') and parseExpression()
						finalNode = ({ kind = 'ForStatement', init = init, goal = goal, step = step } --[=[@as ForStatement]=])
						break
					end
					---@type Identifier[]
					local left = { (firstExpression --[=[@as Identifier]=]) }
					if lastValue == ',' then
						while lastValue == ',' do
							local variable = parseIdentifier()
							if variable.kind ~= 'Identifier' then
								throw('<identifier> expected')
							end
							left[#left + 1] = (variable--[=[@as Identifier]=])
							parse('(%p)', ',')
						end
					end
					if not parse('(%l+)', 'in', 'of') then
						throw("'in', 'of' expected")
					end
					---@type string, Expression
					local operator, right = lastValue, parseExpression()
					finalNode = ({ kind = 'ForEachStatement', left = left, operator = operator, right = right } --[=[@as ForEachStatement]=])
					break
				until true
				if not parse('(%l+)', 'do') then
					throw("'do' expected")
				end
				finalNode.body = parseBlockStatement()
				node = finalNode
				break
			--- BreakStatement
			elseif lastValue == 'break' then
				node = ({ kind = 'BreakStatement' } --[=[@as BreakStatement]=])
				break
			end
		until true
		parse('(%p)', ';')
		return node
	end
	return parseExpressionStatement()
end

local generate = require 'generator'

do
    sourcePath = ...
    sourceContent = io.open(sourcePath, 'r'):read('*a'):match('(.-)%s*$'):gsub('\\([abfnrtv\\\"\'$])', function (c)
		local byte = string.byte(c)
		return string.format('\\u%04x', byte)
	end)
    currentIndex, currentRow = 1, 1

	---@type Program
	local ast = { kind = 'Program', body = {} }
	while not eof() do
		local node = (parseStatement() --[=[@as Statement]=])
		ast.body[#ast.body + 1] = node
		parse('(%p)', ';')
	end

	local code = generate(ast)
	io.write(code, '\n')
	os.exit()
end