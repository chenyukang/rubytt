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
    while (idx < @src.length) and @src[idx].match(/[[:alpha:]0-9_@$\?!]/)
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


  def find_locations(obj)
    def find1(obj)
      if obj.is_a?(Hash)
        #if obj[:type] == :binary and not obj[:left]
        #  puts "problem obj: #{obj.inspect}"
        #end
        ret = {}
        whole_start = nil
        whole_end = nil
        start_line = nil
        end_line = nil

        obj.each do |k, v|
          if k == :location
            start_idx = node_start(v)
            end_idx = ident_end(start_idx)
            ret[:start] = start_idx
            ret[:end] = end_idx
            ret[:start_line] = v[0]
            ret[:end_line] = v[1]
            whole_start = start_idx
            whole_end = end_idx
            start_line = v[0]
            end_line = v[1]
          else
            new_node, start_idx, end_idx, line_start, line_end = find1(v)
            ret[k] = new_node

            if start_idx && (!whole_start || whole_start > start_idx)
              whole_start = start_idx
              start_line = line_start
            end

            if end_idx && (!whole_end || whole_end < end_idx)
              whole_end = end_idx
              end_line = line_end
            end
          end
        end

        if whole_start
          # push whole_end to 'end' keyword
          if [:module, :class, :def, :lambda, :if, :begin, :while, :for]
              .include?(obj[:type]) and not obj[:mod]
            locator = whole_end
            while locator <= @src.length and
                not 'end'.eql? @src[locator .. locator + 'end'.length-1]
              locator += 1
            end
            if 'end'.eql? @src[locator .. locator + 'end'.length-1]
              whole_end = locator + 'end'.length
            end
          end

          ret[:start] = whole_start
          ret[:end] = whole_end
          ret[:start_line] = start_line
          ret[:end_line] = end_line

          # insert docstrings for node if any
          if [:module, :class, :def].include?(ret[:type])
            doc = @docs[start_line]
            if doc
              ret[:doc] = doc
            end
          end
        end
        return ret, whole_start, whole_end, start_line, end_line

      elsif obj.is_a?(Array)
        ret = []
        whole_start = nil
        whole_end = nil

        for v in obj
          new_node, start_idx, end_idx, line_start, line_end = find1(v)
          ret.push(new_node)
          if  start_idx && (!whole_start || whole_start > start_idx)
            whole_start = start_idx
            start_line = line_start
          end

          if end_idx && (!whole_end || whole_end < end_idx)
            whole_end = end_idx
            end_line = line_end
          end
        end

        return ret, whole_start, whole_end, start_line, end_line
      else
        return obj, nil, nil, nil, nil
      end
    end

    node, _, _, _, _ = find1(obj)
    node
  end

  def convert_array(arr)
    arr.map { |x| convert(x) }
  end

  def convert_when(exp, value)
    if exp[0] == :when
      if value
        test = {
            :type => :binary,
            :op => op(:in),
            :left => value,
            :right => args_to_array(convert(exp[1]))
        }
      else
        test = args_to_array(convert(exp[1]))
      end
      ret = {
          :type => :if,
          :test => test,
          :body => convert(exp[2]),
      }
      if exp[3]
        ret[:else] = convert_when(exp[3], value)
      end
      ret
    elsif exp[0] == :else
      convert(exp[1])
    end
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

