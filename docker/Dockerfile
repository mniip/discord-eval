FROM debian:11-slim

COPY buildghc /buildghc

RUN apt-get update && apt-get install -y \
		autoconf \
		build-essential \
		curl \
		debootstrap \
		g++ \
		gcc \
		git \
		libgmp-dev \
		libncurses-dev \
		python3 \
		zlib1g-dev && \
	debootstrap --variant=minbase --include=libgmp-dev bullseye /sandbox/ && \
	rmdir /sandbox/boot /sandbox/home /sandbox/media /sandbox/mnt /sandbox/opt /sandbox/srv && \
	rm -r /sandbox/dev /sandbox/etc /sandbox/proc /sandbox/root /sandbox/run /sandbox/sys /sandbox/var && \
	\
	curl https://downloads.haskell.org/~ghcup/0.1.19.2/x86_64-linux-ghcup-0.1.19.2 -o /ghcup && \
	chmod +x /ghcup && \
	/ghcup install ghc 9.2.8 && \
	/ghcup install cabal 3.10.1.0 && \
	export CABAL_DIR=/buildghc/ && \
	export CABAL="$(/ghcup whereis cabal 3.10.1.0)" && \
	export GHC="$(/ghcup whereis ghc 9.2.8)" && \
	"$CABAL" update &&\
	"$CABAL" install alex happy -w "$GHC" --installdir=/buildghc/ && \
	git clone https://gitlab.haskell.org/ghc/ghc.git --branch ghc-9.6.2-release --depth 1 /buildghc/ghc/ && \
	cd /buildghc/ghc && \
	git submodule update --recursive --init && \
	git apply /buildghc/ghc-patch/*.patch && \
	./boot && \
	./configure --disable-large-address-space --prefix=/sandbox/usr GHC="$GHC" ALEX=/buildghc/alex HAPPY=/buildghc/happy && \
	export LANG=C.UTF-8 && \
	./hadrian/build -j16 install --docs=no-sphinx --prefix=/sandbox/usr && \
	cd / && \
	rm -r /buildghc/ghc && \
	git clone https://github.com/haskell/cabal.git --branch cabal-install-v3.10.1.0 --depth 1 /buildghc/Cabal/ && \
	"$CABAL" user-config init -f -a "symlink-bindir: /buildghc/" && \
	cd /buildghc/Cabal && \
	"$CABAL" install cabal-install -j16 -w "$GHC" && \
	cd / && \
	rm -r /buildghc/Cabal

RUN CABAL_DIR=/buildghc/ /buildghc/bin/cabal v1-install -j16 -w /sandbox/usr/bin/ghc --prefix=/sandbox/usr --haddock-all --disable-library-profiling --global \
	adjunctions \
	aeson \
	attoparsec \
	bifunctors \
	binary \
	conduit \
	constraints \
	criterion \
	directory \
	dlist \
	free \
	hashable \
	kan-extensions \
	lens \
	lens-aeson \
	linear \
	megaparsec \
	mono-traversable \
	numbers \
	parsec \
	pretty-simple \
	primitive \
	process \
	profunctors \
	QuickCheck \
	random \
	recursion-schemes \
	reflection \
	semialign \
	semigroupoids \
	split \
	stm \
	strict \
	tagged \
	tasty \
	th-abstraction \
	these \
	unliftio \
	unordered-containers \
	vector \
	vector-algorithms

RUN git clone https://github.com/mniip/discord-eval.git --depth 1 /discord-eval && \
	cd /discord-eval && \
	export CABAL_DIR=/buildghc/ && \
	export CABAL="$(/ghcup whereis cabal 3.10.1.0)" && \
	export GHC="$(/ghcup whereis ghc 9.2.8)" && \
	"$CABAL" install -j16 -w "$GHC" --installdir=/usr/bin --install-method=copy && \
	git clone https://github.com/mniip/sandbox.git --depth 1 /jail && \
	cd /jail && \
	make && \
	touch /eval.log /input && \
	chown nobody /eval.log /input /jail

RUN apt-get purge -y \
		autoconf \
		debootstrap \
		build-essential \
		curl \
		g++ \
		gcc \
		git \
		libgmp-dev \
		libncurses-dev \
		python3 \
		zlib1g-dev && \
	apt-get autoremove -y && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/* && \
	/ghcup nuke && \
	cd / && \
	rm -r /buildghc /discord-eval /ghcup

COPY gcc /sandbox/usr/bin/gcc
COPY runghci /sandbox/usr/lib/runghci
COPY eval.conf jail.conf /

CMD su nobody -s /bin/sh -c 'LANG=C.UTF-8 exec /usr/bin/discord-eval'
