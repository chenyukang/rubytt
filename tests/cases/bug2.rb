class String

  def conv_to_valid
    if ActiveSupport::Inflector.transliterate(self) == self
      self
    else
      conved_chars = ActiveSupport::Inflector.transliterate(self)
      self.chars.map.with_index { |c, i|
        (c == conved_chars[i] ? c : " ")
      }.join.strip
    end
  end

end
