from nltk.parse.corenlp import CoreNLPServer
import os
# The server needs to know the location of the following files:
# #   - stanford-corenlp-X.X.X.jar 
# #   - stanford-corenlp-X.X.X-models.jar 

main_dir = os.path.dirname(os.path.realpath(__file__))
STANFORD = os.path.join(main_dir, "models", "stanford-corenlp-full-2018-10-05") 
# Create the server

server = CoreNLPServer(
    os.path.join(STANFORD, "stanford-corenlp-3.9.2.jar"),    
    os.path.join(STANFORD, "stanford-corenlp-3.9.2-models.jar")
    ) 
# Start the server in the background 
server.start()

