local generateExpression, generateStatement

---@param lvl integer
---@param head string
---@param body string[]
---@param tail string
---@param sep? string
local function generate (lvl, head, body, tail, sep)
    ---@type string[]
	local parts = {}
	for _, part in ipairs(body) do
		parts[#parts + 1] = string.rep('\t', lvl + 1) .. part
	end
	return head .. '\n' .. table.concat(parts, sep or '\n') .. '\n' .. string.rep('\t', lvl or 0) .. tail
end

---@type table<string, true>
local reservedLuaKeywords = {
	['and'] = true, ['break'] = true, ['do'] = true,
	['else'] = true, ['elseif'] = true, ['end'] = true,
	['false'] = true, ['for'] = true, ['function'] = true,
	['if'] = true, ['in'] = true, ['local'] = true,
	['nil'] = true, ['not'] = true, ['or'] = true,
	['repeat'] = true, ['return'] = true, ['then'] = true,
	['true'] = true, ['until'] = true, ['while'] = true
}

---@type table<boolean, string>
local booleans = { [true] = 'true', [false] = 'false' }

---@param node Expression
---@param lvl? integer
---@return string?
function generateExpression(node, lvl)
    local kind, lastLevel = node.kind, lvl or 0
    if kind == 'GroupExpression' then
        ---@cast node GroupExpression
		return string.format('(%s)', generateExpression(node.expression))
	elseif kind == 'Identifier' then
        ---@cast node Identifier
		return node.value
	elseif kind == 'Literal' then
        ---@cast node Literal
        local typeof = type(node.value)
        if typeof == 'string' then
            return node.value
		elseif typeof == 'number' then
			return tostring(node.value)
        elseif typeof == 'boolean' then
            return booleans[node.value]
        end
		return 'nil'
	elseif kind == 'RecordExpression' then
		---@cast node RecordExpression
		---@type string[], boolean
		local entries, associative = {}, false
		for i, entry in ipairs(node.entries) do
			local field = entry.field and generateExpression(entry.field)
			local key, value = entry.field and (type(entry.field.value) == 'string' and string.sub(field, 2, -2) or field), generateExpression(entry.property, lastLevel + 1)
			local pattern = (reservedLuaKeywords[key] and '[ "%s" ] = %s') or (entry.field and type(entry.field.value) == 'number' and '[ %s ] = %s') or '%s = %s'
			associative, entries[i] = entry.field and associative, key and string.format(pattern, key, value) or value
		end
		if node.destructure then
			return table.concat(entries, ', ')
		end
		if #entries == 0 then
			return '{}'
		end
		return associative and generate(lastLevel, '{', entries, '}', ',\n') or '{ ' .. table.concat(entries, ', ') .. ' }'
	elseif kind == 'CallExpression' then
		---@cast node CallExpression
		---@type string, string[]
		local callee, arguments = generateExpression(node.callee), {}
		for i, argument in ipairs(node.arguments) do
			arguments[i] = generateExpression(argument, lastLevel)
		end
		return string.format('%s(%s)', callee, table.concat(arguments, ', '))
	elseif kind == 'MemberExpression' then
		---@cast node MemberExpression
		local record, id = generateExpression(node.record), generateExpression(node.id)
		if node.computed then
			return string.format('%s[%s]', record, id)
		end
		return record .. node.indexer .. id
	elseif kind == 'UnaryExpression' then
		---@cast node UnaryExpression
		local operator, argument = node.operator, generateExpression(node.argument, lastLevel + 1)
		if operator == 'typeof' then
			return string.format('type(%s)', argument)
		elseif operator == '#' or operator == 'sizeof' then
			return string.format('#%s', argument)
		elseif operator == '...' then
			return string.format('table.unpack(%s)', argument)
		elseif operator == '-' then
			return string.format('-%s', argument)
		end
		return string.format('%s %s', operator, argument)
	elseif kind == 'FunctionExpression' then
		---@cast node FunctionExpression
		---@type string[], string[]
		local parameters, body = {}, generateStatement(node.body, kind, lastLevel + 1)
		for i, parameter in ipairs(node.parameters) do
			parameters[i] = generateExpression(parameter)
		end
		local head = string.format('function (%s)', table.concat(parameters, ', '))
		return generate(lastLevel, head, body, 'end')
	elseif kind == 'BinaryExpression' then
		---@cast node BinaryExpression
		local left, operator, right = generateExpression(node.left), node.operator, generateExpression(node.right)
		if node.operator == 'is' then
			return string.format('type(%s) == %s', left, right)
		elseif operator == '<>' then
			operator = '~='
		end
		return left .. string.format(' %s ', operator) .. right
	elseif kind == 'UpdateExpression' then
		---@cast node UpdateExpression
		local operator, argument = (node.operator == '++') and '+' or '-', generateExpression(node.argument)
		return argument .. ' = ' .. argument .. ' ' .. operator .. ' 1'
	elseif kind == 'AssignmentExpression' then
		---@cast node AssignmentExpression
		local left, right = generateExpression(node.left), generateExpression(node.right, lastLevel)
		if node.operator ~= '=' then
			right = left .. node.operator .. right
		end
		return left .. ' = ' .. right
	end
end

---@param node Statement
---@param parent? string
---@param lvl? integer
function generateStatement (node, parent, lvl)
	local kind, lastLevel = node.kind, lvl or 0
	if kind == 'VariableDeclaration' then
		---@cast node VariableDeclaration
		---@type string[], string[]
		local ids, inits = {}, {}
		for i, declaration in ipairs(node.declarations) do
			ids[i] = declaration.id and generateExpression(declaration.id)
			if declaration.init and i > 1 and not inits[i - 1] then
				inits[i - 1] = 'nil'
			end
			inits[i] = declaration.init and generateExpression(declaration.init, lastLevel);
		end
		if #inits == 0 then
			return 'local ' .. table.concat(ids, ', ')
		end
		return string.format('local %s = %s', table.concat(ids, ', '), table.concat(inits, ', '))
	elseif kind == 'FunctionDeclaration' then
		---@cast node FunctionDeclaration
		local name = generateExpression(node.name)
		---@type string[], string[]
		local parameters, body = {}, generateStatement(node.body, nil, lastLevel)
		for i, parameter in ipairs(node.parameters) do
			parameters[i] = generateExpression(parameter)
		end
		local storage = parent and 'local ' or ''
		local head = string.format('%sfunction %s (%s)', storage, name, table.concat(parameters, ', '))
		return #body > 0 and generate(lastLevel, head, body, 'end') or string.format('%s end', head)
	elseif kind == 'ReturnStatement' then
		---@cast node ReturnStatement
		---@type string[]
		local arguments = {}
		for i, argument in ipairs(node.arguments) do
			arguments[i] = generateExpression(argument)
		end
		return string.format('return %s', table.concat(arguments, ', '))
	elseif kind == 'IfStatement' then
		---@cast node IfStatement
		---@type string, IfStatement|BlockStatement
		local block, latest = 'if', node
		while latest do
			repeat
				---@type string?, string[]
				local head, body = latest.test and generateExpression(latest.test, lastLevel), generateStatement(latest.consequent or latest, kind, lastLevel)
				---@type string, string
				local first, second = string.format(head and ' %s then' or 'else', head), (latest.alternate and latest.alternate.test) and 'elseif' or ''
				block, latest = block .. generate(lastLevel, first, body, '') .. second, latest.alternate
			until not latest
		end
		return block .. 'end'
	elseif kind == 'DoStatement' then
		---@cast node DoStatement
		---@type string[], string
		local body, test = generateStatement(node.body, kind, lastLevel), node.test and generateExpression(node.test)
		if node.loop == 'while' then
			return generate(lastLevel, string.format('while %s do', test), body, 'end')
		elseif node.loop == 'until' then
			return generate(lastLevel, 'repeat', body, string.format('until %s', test))
		end
		return generate(lastLevel, 'do', body, 'end')
	elseif kind == 'ForStatement' or kind == 'ForEachStatement' then
		---@cast node DoStatement
		local body = generateStatement(node.body, nil, lastLevel) --[=[@as string[]]=]
		if kind == 'ForStatement' then
			---@cast node ForStatement
			local init = (generateExpression(node.init) --[=[@as string]=])
			local goal, step = generateExpression(node.goal), node.step and generateExpression(node.step)
			return generate(lastLevel, string.format('for %s, %s%s do', init, goal, step and string.format(', %s', step) or ''), body, 'end')
		end
		---@cast node ForEachStatement
		---@type string[], string
		local left, right = { (node.operator == 'of') and '_' or '' }, generateExpression(node.right)
		for _, variable in ipairs(node.left) do
			left[#left + 1] = generateExpression(variable) --[=[@as string]=]
		end
		return generate(lastLevel, string.format('for %s in %s(%s) do', table.concat(left, ', '), (node.operator == 'in') and 'pairs' or 'ipairs', right), body, 'end')
	elseif kind == 'BreakStatement' then
		return 'break'
	elseif kind == 'BlockStatement' then
		---@cast node BlockStatement
		---@type string[]
		local body = {}
		for i, statement in ipairs(node.body) do
			body[i] = generateStatement(statement, node.kind, lastLevel + 1)
		end
		return body
	end
	---@cast node ExpressionStatement
	return generateExpression(node.expression, lastLevel)
end


---@type fun(ast: Program): string
---@param ast Program
---@return string
return function (ast)
	---@type string[], string[]
	local output, exports = {}, {}
	for index, node in ipairs(ast.body) do
		output[index] = generateStatement(node)
	end
	return table.concat(output, '\n')
end