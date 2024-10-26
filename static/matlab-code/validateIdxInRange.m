function validateIdxInRange(ss, idx, dim)
n = size(ss, dim);
if any(idx < 1) || any(idx > n)
    eid = 'Index:OutOfRange';
    msg = sprintf("Index must be in [1, %d] of dimension %d", n, dim);
    throwAsCaller(MException(eid, msg));
end
end