function (leftHandSide, rightHandSide) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rightHandSides <- chain_parts[["rightHandSides"]]
    leftHandSide <- chain_parts[["leftHandSide"]]
    env[["_function_list"]] <- lapply(1:length(rightHandSides), function(i) wrap_function(rightHandSides[[i]],
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value,
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(leftHandSide)) {
        env[["_fseq"]]
    }
    else {
        env[["_leftHandSide"]] <- eval(leftHandSide, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_leftHandSide`)), env,
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", leftHandSide, result[["value"]]), parent,
                parent)
        }
        else {
            if (result[["visible"]])
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}
