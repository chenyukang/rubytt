## a bug for function with lambda
class AstSimplifier

  def initialize(filename)
    find_line_starts
  end

  def find_line_starts
    res = ""
    lines.each { |line|
      res += line
    }
  end

end

