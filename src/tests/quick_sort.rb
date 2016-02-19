def qsort lst
  return [] if lst.size() == 0
  less, more = left.partition {|x| x < first}
  return qsort(less) + [first] + qsort(more)
end

