from optparse import OptionParser

parser = OptionParser()
parser.add_option("-p", "--profile", dest="profile", default=False, action="store_true",
                  help="Enable profiling of pyhi")

(options, args) = parser.parse_args()
