local generate = require('generator')
local sourcePath, sourceContent, lastIndex, lastValue, currentIndex, currentRow, parseExpression, parseStatement
function match (a, ...)
	for b = 1, select('#', ...) do
		if a == select(b, ...) then
			return a
		end
	end
end
function eof ()
	return not sourceContent:byte(currentIndex)
end
function parse (p, ...)
	local toIndex, firstValue
	lastIndex, toIndex, firstValue, lastValue = sourceContent:find("^(%s*)" .. tostring(p) .. "", currentIndex)
	if select('#', ...) == 0 and lastValue or match(lastValue, ...) then
		local _, rowCount = firstValue:gsub('[\n\r]', '')
		currentIndex, currentRow = toIndex + 1, currentRow + rowCount
		return lastValue
	end
end
function throw (m, from, pattern)
	pattern, currentIndex = pattern or '(%S+)', from or lastIndex or currentIndex
	local hint = parse(pattern) and "\"" .. tostring(lastValue) .. "\"" or '<eof>'
	io.write("[" .. tostring(sourcePath) .. "] smc:" .. tostring(currentRow) .. " " .. tostring(m) .. " near " .. tostring(hint) .. ".\n")
	os.exit()
end
local reservedKeywords = { [ "true" ] = true, [ "false" ] = true, undef = true, [ "and" ] = true, [ "or" ] = true, is = true, [ "not" ] = true, sizeof = true, typeof = true, var = true, def = true, [ "return" ] = true, [ "if" ] = true, [ "then" ] = true, [ "else" ] = true, [ "do" ] = true, [ "while" ] = true, [ "until" ] = true, [ "for" ] = true, to = true, step = true, of = true, [ "in" ] = true, [ "break" ] = true, [ "end" ] = true }
local escapedCharacters = { [ 34 ] = '\"', [ 36 ] = '$', [ 39 ] = '\'', [ 92 ] = '\\', [ 97 ] = 'a', [ 98 ] = 'b', [ 102 ] = 'f', [ 110 ] = 'n', [ 114 ] = 'r', [ 115 ] = 't', [ 118 ] = 'v' }
function parseIdentifier ()
	if parse('(%p)', '(') then
		local expression = parseExpression()
		if not parse('(%p)', ')') then
			throw("')' expected")
		end
		return { kind = 'GroupExpression', expression = expression }
	elseif not parse('([_%a][_%w]*)') or reservedKeywords[lastValue] then
		throw('unexpected symbol')
	end
	return { kind = 'Identifier', value = lastValue }
end
function parseTemplate (str)
	local newStr = str:gsub('${(.-)}', function (t)
		return "\" .. tostring(" .. tostring(t) .. ") .. \""
	end)
	return "\"" .. tostring(newStr) .. "\""
end
function parseLiteral ()
	local value, startIndex = nil, currentIndex
	if parse("'([^']*)") then
		value = string.format("'%s'", lastValue):gsub('\\u(%x+)', function (c)
			local n = tonumber(c, 16)
			return "\\" .. tostring(escapedCharacters[n] or string.char(n)) .. ""
		end)
		if not parse('(%p)', "'") then
			throw('malformed string', startIndex, '([^\n\r]*)')
		end
	elseif parse('"([^"]*)') then
		value = parseTemplate(lastValue):gsub('\\u(%x+)', function (c)
			local n = tonumber(c, 16)
			return "\\" .. tostring(escapedCharacters[n] or string.char(n)) .. ""
		end)
		if not parse('(%p)', '"') then
			throw('malformed string', startIndex, '([^\n\r]*)')
		end
	elseif parse('(%d[%d_]*)') then
		local firstValue, secondValue = lastValue, parse('(%.?%d[_%d]*)') or ''
		if parse('(%p)', '.') then
			throw('malformed number', startIndex)
		end
		value = string.gsub(firstValue .. secondValue, '_', '') + 0
	elseif parse('(&)%f[%x]') then
		if not parse('(%x+)') then
			throw('malformed number', startIndex)
		end
		value = tonumber(lastValue, 16)
	elseif parse('(%l+)', 'true', 'false', 'undef') then
		if lastValue == 'true' then
			value = true
		elseif lastValue == 'false' then
			value = false
		end
	elseif parse('(%p+)%f[^.]', '...') then
		value = lastValue
	else
		return parseIdentifier()
	end
	return { kind = 'Literal', value = value }
end
function parseRecordExpression (dstrc)
	if parse('(%p)', '[') then
		local entries = {}
		if not parse('(%p)', ']') then
			repeat
				local field, property = nil, parseExpression()
				if parse('(%p)', ':', ',') == ':' then
					if property.kind ~= 'Literal' or not match(type(property.value), 'string', 'number') then
						throw('<string> or <number> expected')
					elseif dstrc then
						throw('unexpected symbol')
					end
					field, property = property, parseExpression()
					parse('(%p)', ',')
				end
				entries[#entries + 1] = { kind = 'Entry', field = field, property = property }
			until parse('(%p)', ']') or eof()
			if not lastValue then
				throw("']' expected")
			end
		end
		return { kind = 'RecordExpression', entries = entries, destructure = dstrc }
	end
	return parseLiteral()
end
function parseCallExpression (callee)
	while parse('(%p)', '(') do
		local arguments = {}
		if not parse('(%p)', ')') then
			repeat
				arguments[#arguments + 1] = parseExpression()
			until not parse('(%p)', ',') or eof()
			if not parse('(%p)', ')') then
				throw("')' expected")
			end
		end
		callee = { kind = 'CallExpression', callee = callee, arguments = arguments }
	end
	return callee
end
function parseMemberExpression (dstrc)
	local lastNode = parseRecordExpression(dstrc)
	if lastNode.kind == 'Identifier' then
		lastNode = parseCallExpression(lastNode)
		while parse('(%p)%f[#%P]', '.', ':', '[') do
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
			lastNode = parseCallExpression({ kind = 'MemberExpression', record = lastNode, id = id, computed = computed, indexer = indexer })
			if lastNode.kind == 'MemberExpression' and lastNode.indexer == ':' then
				throw('function arguments expected')
			end
		end
	end
	return lastNode
end
function parseBlockStatement ()
	local body = {}
	while not parse('(%l+)', 'end') and not eof() do
		body[#body + 1] = parseStatement()
	end
	if not lastValue then
		throw("'end' expected")
	end
	return { kind = 'BlockStatement', body = body }
end
function parseFunctionExpression ()
	if parse('(%l+)', 'def') then
		local parameters = {}
		if not parse('(%p)', '(') then
			throw("'(' expected")
		elseif not parse('(%p)', ')') then
			repeat
				local parameter = parseIdentifier()
				if parameter.kind ~= 'Identifier' then
					throw('<identifier> expected')
				end
				parameters[#parameters + 1] = parameter
			until not parse('(%p)', ',') or eof()
			if not parse('(%p)', ')') then
				throw("')' expected")
			end
		end
		return { kind = 'FunctionExpression', parameters = parameters, body = parseBlockStatement() }
	end
	return parseMemberExpression()
end
function parseUnaryExpression ()
	while parse('(%l+)', 'not', 'sizeof', 'typeof') or parse('(%p+)', '#', '...', '-') do
		return { kind = 'UnaryExpression', operator = lastValue, argument = parseFunctionExpression() }
	end
	return parseFunctionExpression()
end
function parseMultiplicativeExpression ()
	local left = parseUnaryExpression()
	while parse('(%p+)', '*', '/', '^', '%', '..') do
		left = { kind = 'BinaryExpression', left = left, operator = lastValue, right = parseUnaryExpression() }
	end
	return left
end
function parseAdditiveExpression ()
	local left = parseMultiplicativeExpression()
	while parse('(%p)', '+', '-') do
		left = { kind = 'BinaryExpression', left = left, operator = lastValue, right = parseMultiplicativeExpression() }
	end
	return left
end
function parseComparisonExpression ()
	local left = parseAdditiveExpression()
	while parse('(%p+)', '==', '<>', '<', '<=', '>', '>=') do
		left = { kind = 'BinaryExpression', left = left, operator = lastValue, right = parseAdditiveExpression() }
	end
	return left
end
function parseLogicalExpression ()
	local left = parseComparisonExpression()
	while parse('(%l+)', 'and', 'or') do
		left = { kind = 'BinaryExpression', left = left, operator = lastValue, right = parseComparisonExpression() }
	end
	return left
end
function parseExpression ()
	return parseLogicalExpression()
end
function parseExpressionStatement ()
	local expression = parseMemberExpression(true)
	if not match(expression.kind, 'MemberExpression', 'CallExpression', 'RecordExpression', 'GroupExpression', 'Identifier') then
		throw('syntax error')
	elseif expression.kind == 'GroupExpression' then
		expression = parseCallExpression(expression)
	elseif expression.kind ~= 'CallExpression' then
		if parse('(%p+)%f[^+-]', '++', '--') then
			expression = { kind = 'UpdateExpression', argument = expression, operator = lastValue }
		else
			local right, operator = nil, parse('(%p+)%f[=]', '+', '-', '*', '/', '//', '^', '%', '..')
			if (expression.kind == 'RecordExpression' and operator) or not parse('(%p)', '=') then
				throw("'=' expected")
			end
			operator, right = operator or lastValue, parseExpression()
			if expression.kind == right.kind then
				right.destructure = true
			end
			expression = { kind = 'AssignmentExpression', left = expression, operator = operator, right = right }
		end
	end
	parse('(%p)', ';')
	return { kind = 'ExpressionStatement', expression = expression }
end
function parseStatement ()
	local lastNode
	if parse('(%p+)', '//') then
		parse('([^\n\r]*)')
	elseif parse('(%l+)', 'var', 'def', 'return', 'if', 'do', 'while', 'for', 'break') == 'var' then
		local declarations = {}
		repeat
			local init, id = nil, parseRecordExpression(true)
			if not match(id.kind, 'RecordExpression', 'Identifier') then
				throw('<identifier> expected')
			elseif parse('(%p)', '=') then
				init = parseExpression()
			end
			declarations[#declarations + 1] = { kind = 'VariableDeclarator', id = id, init = init }
		until not parse('(%p)', ',') or eof()
		lastNode = { kind = 'VariableDeclaration', declarations = declarations }
	elseif lastValue == 'def' then
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
				elseif parameters[#parameters] and parameters[#parameters].value == '...' and parameter.value == '...' then
					throw("')' expected")
				end
				parameters[#parameters + 1] = parameter
			until not parse('(%p)', ',') or eof()
			if not parse('(%p)', ')') then
				throw("')' expected")
			end
		end
		lastNode = { kind = 'FunctionDeclaration', name = name, parameters = parameters, body = parseBlockStatement() }
	elseif lastValue == 'return' then
		local arguments = {}
		repeat
			arguments[#arguments + 1] = parseExpression()
		until not parse('(%p)', ',') or eof()
		lastNode = { kind = 'ReturnStatement', arguments = arguments }
	elseif lastValue == 'if' then
		local finalNode = {}
		local currentNode = finalNode
		while lastValue == 'if' or lastValue == 'else' do
			local testExpression, currentValue, blockStatement, isNested, targetKey = nil, lastValue, { kind = 'BlockStatement', body = {} }, false, 'consequent'
			if currentValue == 'else' then
				isNested, targetKey = parse('(%l+)', 'if'), 'alternate'
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
			currentNode[targetKey] = isNested and { kind = 'IfStatement', test = testExpression, consequent = blockStatement } or blockStatement
			currentNode = (targetKey == 'alternate') and currentNode[targetKey] or currentNode
			currentNode.kind, currentNode.test = testExpression and 'IfStatement' or 'BlockStatement', testExpression
		end
		lastNode = finalNode
	elseif lastValue == 'do' or lastValue == 'while' then
		local keyword, body, loop, test = lastValue, {}
		if keyword == 'while' then
			loop, test = keyword, parseExpression()
			if not parse('(%l+)', 'do') then
				throw("'do' expected")
			end
		end
		while not parse('(%l+)', 'until', 'end') and not eof() do
			body[#body + 1] = parseStatement()
		end
		if not lastValue or (keyword == 'while' and lastValue == 'until') then
			throw("'end' expected")
		elseif lastValue == 'until' then
			loop, test = lastValue, parseExpression()
		end
		lastNode = { kind = 'DoStatement', test = test, loop = loop, body = { kind = 'BlockStatement', body = body } }
	elseif lastValue == 'for' then
		local firstExpression = parseIdentifier()
		if firstExpression.kind ~= 'Identifier' then
			throw('<variable> expected')
		elseif parse('(%p)', ',', '=') == '=' then
			local init = { kind = 'AssignmentExpression', left = firstExpression, operator = lastValue, right = parseExpression() }
			if not parse('(%l+)', 'to') then
				throw("'to' expected")
			end
			local goalExpression, stepExpression = parseExpression(), parse('(%l+)', 'step') and parseExpression()
			lastNode = { kind = 'ForStatement', init = init, goal = goalExpression, step = stepExpression }
		else
			local left = { firstExpression }
			if lastValue == ',' then
				while lastValue == ',' do
					local variable = parseIdentifier()
					if variable.kind ~= 'Identifier' then
						throw('<identifier> expected')
					end
					left[#left + 1] = variable
					parse('(%p)', ',')
				end
			end
			if not parse('(%l+)', 'in', 'of') then
				throw("'in', 'of' expected")
			end
			local operator, right = lastValue, parseExpression()
			lastNode = { kind = 'ForEachStatement', left = left, operator = operator, right = right }
		end
		if not parse('(%l+)', 'do') then
			throw("'do' expected")
		end
		lastNode.body = parseBlockStatement()
	elseif lastValue == 'break' then
		lastNode = { kind = 'BreakStatement' }
	else
		return parseExpressionStatement()
	end
	parse('(%p)', ';')
	return lastNode
end
do
	sourcePath = ...
	sourceContent = io.open(sourcePath, 'r'):read('*a'):match('(.-)%s*$'):gsub('\\([abfnrtv\\\"\'$])', function (c)
		local byte = string.byte(c)
		return string.format('\\u%04x', byte)
	end)
	currentIndex, currentRow = 1, 1
	local ast = { kind = 'Program', body = {} }
	while not eof() do
		local lastNode = parseStatement()
		ast.body[#ast.body + 1] = lastNode
		parse('(%p)', ';')
	end
	local code = generate(ast)
	io.write("" .. tostring(code) .. "\n")
	os.exit()
end
