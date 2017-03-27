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

	@doc """
	tuple test
	
	"""

	def tuple(list) do
		List.to_tuple(list)
	end

	def clean({thez, ind}, path) do
		thez
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
			{ flow, string } -> cmdParse( { flow, string }, path)
			{ cmd, loc, qty } -> cmdParse( { cmd, loc, qty }, path )
			{ g } -> arithmetic( g, ind )
		end
	end

	def cmdParse({a,b,c}, path) do
		case b do
			"local" -> _cmdParse( {a, "@LCL\nA=M", c } )	
			"argument" -> _cmdParse( { a, "@ARG\nA=M", c } )
			"this" -> _cmdParse( { a, "@THIS\nA=M", c } )
			"that" -> _cmdParse( { a, "@THAT\nA=M", c } )
	 		"temp" -> _cmdParse( { a, "@R5", c } )
	 		"pointer" -> _cmdParse( { a, "@R3", c } )
			"static" -> _cmdParse( { a, "@#{Path.basename(path) |> Path.rootname}.#{c}", 0 } )
		end
	end

	def cmdParse( {flow, string }, path ) do
		case flow do
			"label" -> "(#{string})\n"
			"goto" -> "@" <> string <> "\n0;JMP\n"
			#assuming the prior conditional left a boolean
			"if-goto" -> "@SP\nA=M-1\nD=M\n@1\nD=D+A\n@#{string}\nD;JEQ\n"
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

	def writeFile( string, path ) do
		finalstr =  string <> "\n" <> String.trim_trailing(@endloop)
		IO.write finalstr
		File.write("#{path}.asm", finalstr )
	end

end

Vmtranslator.main(System.argv())