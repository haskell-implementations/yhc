def print_config(msg, two_dee_iterable):
    # this function is handy and can be used for other configuration-printing tasks
    print
    print msg
    print
    for key, val in two_dee_iterable:
        print "    %-20s %s" % (key, val)
    print

def config_h_build(target, source, defines):
    #defines = env["vars"]
    print_config("Generating config.h with the following settings:",
                  defines.items())

    if isinstance(source, str):
        source = [source]
        target = [target]

    for a_target, a_source in zip(target, source):
        config_h_in = file(str(a_source), "r")
        text = config_h_in.read() % defines
        config_h_in.close()

        config_h = file(str(a_target), "w")
        config_h.write(text)
        config_h.close()
