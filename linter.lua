--[=[
	function myFunction (myParameter)
		return myParameter + 2;
	end

	{
		'kind': 'Program',
		'body': {
			'kind': 'BlockStatement',
			'body': [
				{
					'kind': 'ReturnStatement',
					'arguments': [
						{
							'kind': 'BinaryExpression',
							'left': {
								'kind': 'Identifier',
								'value'; 'myParameter'
							},
							'operator': '=',
							'right': {
								'kind': 'Literal',
								'value': 2
							}
						}
					]
				}
			]
		}
	}
-]=]