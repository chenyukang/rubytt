require 'ripper'
require 'pp'
require 'json'
require 'optparse'


# --------------------- utils ---------------------
def banner(s)
  puts "\033[93m#{s}:\033[0m"
end


class AstSimplifier

  def initialize(filename)
    @filename = filename

    f = File.open(filename, 'rb')
    @src = f.read
    f.close

    detected_enc = detect_encoding(@src)
    if detected_enc
      begin
        @src.force_encoding(detected_enc)
      rescue
        @src.force_encoding('utf-8')
      end
    else
      # default to UTF-8
      @src.force_encoding('utf-8')
    end

    @src.encode('utf-8',
                {:undef => :replace,
                 :invalid => :replace,
                 :universal_newline => true}
    )

    @line_starts = [0]
    find_line_starts
    find_docs
  end


  def detect_encoding(s)
    # take first two lines
    header = s.match('^.*\n?.*\n?')
    if header and header[0]
      matched = header[0].match('^\s*#.*coding\s*[:=]\s*([\w\d\-]+)')
      if matched and matched[1]
        matched[1]
      end
    end
  end


  # initialize the @line_starts array
  # used to convert (line,col) location to (start,end)
  def find_line_starts
    lines = @src.split(/\n/)
    total = 0
    lines.each { |line|
      total += line.length + 1 # line and \n
      @line_starts.push(total)
    }
  end


  def find_docs
    @docs = {}
    lines = @src.split(/\n/)
    first_line = nil
    current_line = 0
    accum = []

    lines.each { |line|
      matched = line.match('^\s*#\s*(.*)')
      if matched
        accum.push(matched[1])
        if !first_line
          first_line = current_line
        end
      elsif !accum.empty?
        doc = {
            :type => :string,
            :id => accum.join('\n'),
        }
        @docs[current_line+1] = doc
        @docs[first_line-1] = doc
        accum.clear
        first_line = nil
      end

      current_line += 1
    }
  end


  def node_start(loc)
    line = loc[0]
    col = loc[1]
    @line_starts[line-1] + col
  end


  def ident_end(start_idx)
    if @src[start_idx] == '[' and @src[start_idx + 1] == ']'
      return start_idx + 2
    end
    idx = start_idx
    while idx < @src.length and @src[idx].match /[[:alpha:]0-9_@$\?!]/
      idx += 1
    end
    idx
  end

  def simplify
    tree = Ripper::SexpBuilder.new(@src).parse

    if $options[:debug]
      banner 'sexp'
      pp tree
    end

    simplified = convert(tree)
    simplified = find_locations(simplified)

    if $options[:debug]
      banner 'simplified'
      pp simplified
    end

    simplified
  end


  def args_to_array(args)
    if args[:type] == :args
      {
          :type => :array,
          :elts => args[:positional]
      }
    else
      args
    end
  end


  def make_string(content, location=nil)
    ret = {
        :type => :string,
        :id => content.force_encoding('utf-8'),
    }
    if location
      ret[:location] = location
    end
    ret
  end


  def op(name)
    {
        :type => :op,
        :name => name
    }
  end


  def negate(exp)
    {
        :type => :unary,
        :op => op(:not),
        :operand => exp
    }
  end

end

# def hash_max_nest(hash)
#   if hash.is_a?(Array)
#     hash.map{ |e| hash_max_nest(e).to_i }.max.to_i + 1
#   elsif hash.is_a?(Hash)
#     hash.values.map{ |s| hash_max_nest(s).to_i }.max.to_i + 1
#   else
#     0
#   end
# end

def parse_dump(input, output, endmark)
  begin
    simplifier = AstSimplifier.new(input)
    hash = simplifier.simplify
    json_string = JSON.pretty_generate(hash, max_nesting: hash_max_nest(hash))
    out = File.open(output, 'wb')
    out.write(json_string)
    out.close
  ensure
    end_file = File.open(endmark, 'wb')
    end_file.close
  end
end


$options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: dump.rb [options]"

  opts.on("-d", "--debug", "debug run") do |v|
    $options[:debug] = v
  end

end.parse!


if ARGV.length > 0
  parse_dump(ARGV[0], ARGV[1], ARGV[2])
end

