

addUpMapFirst    :: [Int] -> [Int]
addUpMapFirst ns = filter greaterOne (map addOne ns)

addUpFilterFirst    :: [Int] -> [Int]
addUpFilterFirst ns = map addOne (filter greaterOne ns)

greaterOne n = n > 1
addOne n = n + 1