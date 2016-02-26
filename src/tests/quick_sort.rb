def qsort lst
  return [] if lst.size() == 0
  # less, more = left.partition {|x| x < first}
  less, more = left.partition ## bug now
  return qsort(less) + [first] + qsort(more)
end

