Setups: 

All following commands should be run in command line,in the directory above 
which the project resites (the tweetmeet sub-directory, in this case)

1) install stack commend line tools
curl -sSL https://get.haskellstack.org/ | sh

2) install yesod command line tool 
stack build yesod-bin cabal-install --install-ghc

3) build libraries
stack build

4) launch server
stack exec -- yesod devel

local page should be available at 
http://localhost:3000/ 

Reference:
http://www.yesodweb.com/page/quickstart
