defmodule Vmtranslator do
	@endloop """
	(END)
	@END
	0;JMP
	"""
	@compare """
	@SP
	M=M-1
	A=M
	D=M
	M=0
	A=A-1
	D=M-D
	M=-1
	"""
	@arithmetic """
	@SP
	A=M-1
	D=M
	M=0
	A=A-1
	"""
	@nd "M=M&D"
	@wr "M=M|D"
	@add "M=M+D"
	@sub "M=M-D"
	@gt "D;JGT"
	@lt "D;JLT"
	@eq "D;JEQ"

	@moduledoc """
 		This is just a test
	"""
	@doc """
	another test

	"""
	def main(args) do
		args |> parse_args |> parsing
	end

  	defp parse_args(args) do
    	# Covering only expected arguments. We can add code to handle cases when unexpected arguments are passed or help text etc.
    	# Checkout OptionParser lib for details. 
    	{_, b, _} = OptionParser.parse(args)
    	c = Path.absname(b, System.cwd!)
    	~s/#{c}/
  	end

	def parsing(file) do
		#worded = ~s/#{file}/
		path = Path.rootname(file)
		File.stream!(file)
			#truthfully it seems I should only need to pass the path on the LAST VM line
			# I may be able to reduce below to Stream.with_index ; THEN Stream.filter_map
			#|> Stream.filter_map( &( String.match?( &1, ~r{^\w.+$} ) ), &clean/1 ) 
			|> Stream.filter( &( String.match?( &1, ~r{^\w.+$} ) )) #this is going to have to clear in-line comments
			|> Stream.with_index
			|> Stream.map( &(clean(&1, path) ) )
			|> Enum.reduce( &( &2 <> &1 ) )
			|> String.trim_trailing
			|> writeFile(path)
	end

	def find_func( _path, function) do
		#this is where I need to find a specific function within a "Class.vm" file
		#this function is definitely NOT finished.  How to know when to stop wth func in class file? -> @return line
		#base_path = Path.absname(path)
		file = ( ( String.split(function, ".") |> hd ) <> ".vm" )
		[_, str] = Regex.run( ~r/function #{function} (.*)return/s, File.read!(file) )
		str
			|> String.split("\r\n")
			|> Stream.filter( &( String.match?( &1, ~r{^\w.+$} ) ))
			|> Stream.with_index
			|> Stream.map( &(clean(&1, function) ) )
			|> Enum.reduce( &( &2 <> &1 ) )
	end

	@doc """
	tuple test
	
	"""

	def tuple(list) do
		List.to_tuple(list)
	end

	def clean({thez, ind}, path) do
		thez
			|> String.split("//", trim: true)
			|> hd
			|> String.trim_trailing
			|> String.split(" ")
			|> tuple
			|> createString
			|> createStringT(ind, path)
	end

	def createString( {a,b,c} ) do
		{a,b, parseInteger(c)}
	end

	def createString( tuple ) do
		tuple
	end

	def parseInteger(st) do
		{a, _}  = Integer.parse(st)
		a
	end

	def createStringT( tuple , ind, path) do
		case tuple do
			{_a,"constant",c} -> """
			@#{c}
			D=A
			@SP
			A=M
			M=D
			@0
			M=M+1
			"""
			{ "function", fn_name, num_locals } -> _function({ "function", fn_name, num_locals }, ind, path )
			{ "call", fn_name, num_args } -> _function({ "call", fn_name, num_args }, ind, path ) #need to locate the vm file
			{ "return" } -> _function({"return"}, path )
			{ flow, string } -> cmdParse( { flow, string }, path)
			{ cmd, loc, qty } -> cmdParse( { cmd, loc, qty }, path, ind )
			{ g } -> arithmetic( g, ind )
		end
	end

	def cmdParse({a,b,c}, path, ind) do
		case b do
			"local" -> _cmdParse( {a, "@LCL\nA=M", c } )	
			"argument" -> _cmdParse( { a, "@ARG\nA=M", c } )
			"this" -> _cmdParse( { a, "@THIS\nA=M", c } )
			"that" -> _cmdParse( { a, "@THAT\nA=M", c } )
	 		"temp" -> _cmdParse( { a, "@R5", c } )
	 		"pointer" -> _cmdParse( { a, "@R3", c } )
			"static" -> _cmdParse( { a, "@#{Path.basename(path) |> Path.rootname}#{ind}.#{c}", 0 } ) #Path.basename(path) |> Path.rootname
		end
	end

	def cmdParse( {flow, string }, _path ) do
		case flow do
			"label" -> "(#{string})\n"
			"goto" -> "@" <> string <> "\n0;JMP\n"
			"if-goto" -> "@SP\nM=M-1\nA=M\nD=M\n@#{string}\nD;JNE\n"
		end
	end

	defp _cmdParse( { "push", loc, qty } ) do 
		"#{loc}\n"<> String.duplicate("A=A+1\n", qty) <> "D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
	end

	defp _cmdParse({"pop", loc, qty}) do 
		"@SP\nM=M-1\nA=M\nD=M\nM=0\n#{loc}\n"<> String.duplicate("A=A+1\n", qty) <> "M=D\n"
	end

	def arithmetic(cmd, ind) do
		 _arithmetic(cmd, ind)
	end

	defp _arithmetic(a, ind) do
		case a do
			"lt" -> @compare <> "@LABEL#{ind}\n" <> @lt <> "\n@SP\nA=M-1\nM=0\n(LABEL#{ind})\n"
			"gt" -> @compare <> "@LABEL#{ind}\n" <> @gt <> "\n@SP\nA=M-1\nM=0\n(LABEL#{ind})\n"
			"eq" -> @compare <> "@LABEL#{ind}\n" <> @eq <> "\n@SP\nA=M-1\nM=0\n(LABEL#{ind})\n"
			"add" -> @arithmetic <> @add <> "\n@SP\nM=M-1\n"
			"sub" -> @arithmetic <> @sub <> "\n@SP\nM=M-1\n"
			"and" -> @arithmetic <> @nd <> "\n@SP\nM=M-1\n"
			"or" -> @arithmetic <> @wr <> "\n@SP\nM=M-1\n"
			"neg" -> "@SP\nA=M-1\nM=-M\n"
			"not" -> "@SP\nA=M-1\nM=!M\n"
		end
	end

	defp _function( { a, b, n_args }, ind, _path ) do
		#base_path = Path.basename(path)
		duple = """
		D=M
		@SP
		A=M
		M=D
		@SP
		M=M+1
		""" 
			#|> String.trim_trailing
		case a do
			"function" -> "@261\nD=A\n@SP\nM=D\n" <> String.duplicate("A=M\nM=0\n@SP\nM=M+1\n", n_args) # function commands, then return
			"call" -> """
			@#{b}$#{ind}
			D=A
			@SP
			A=M
			M=D
			@SP
			M=M+1
			(#{b}$#{ind})
			#{Enum.join(["@LCL", "@ARG", "@THIS", "@THAT"], "\n" <> duple)}
			"""
				<>"@SP\nD=M-1\n"
				<> String.duplicate("D=D-1\n", 5+n_args-1) <> 
			"""
			@ARG
			M=D
			@SP
			D=M
			@LCL
			M=D
			@SP
			""" 
				<> String.duplicate("A=M\nM=0\n@SP\nM=M+1\n", n_args)
				<> ( parse_args( [( String.split(b, ".") |> hd ) <> ".vm"] ) |> find_func(b) ) <> 
			"""
			@LCL
			D=M
			@R5
			M=D
			@R6
			M=D-1
			@4
			D=A
			@R6
			M=M-D
			"""
				<> cmdParse({"pop","argument",0}, "", "FUNC") <>
			"""
			@ARG
			D=A+1
			@SP
			A=M
			M=D
			@R5
			D=M
			D=D-1
			@THAT
			M=D
			D=D-1
			@THIS
			M=D
			D=D-1
			@ARG
			M=D
			D=D-1
			@LCL
			M=D
			@R5
			A=M
			0;JMP
			"""
		end	
	end

	defp _function( { _ }, _path) do
		""
	end

	def writeFile( string, path ) do
		finalstr =  string <> "\n" <> String.trim_trailing(@endloop)
		IO.write finalstr
		File.write("#{path}.asm", finalstr )
	end

end

Vmtranslator.main(System.argv())