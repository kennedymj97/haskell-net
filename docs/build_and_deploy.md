# Haskell Net
Setup on local machine:
1. Install stack
	1. sudo apt-get install haskell-stack
	2. stack upgrade --binary-only
	3. source ~/.profile
2. If get you are not the owner error:
	1. sudo nano ~/.stack/config.yaml
	2. allow-different-user: true
    
**Note: below refers to using an ec2 instance, this will actually be deployed on a gcp compute instance as there is a permanent free tier.**
    
As long as the local machine and the aws machine have the same OS version (Ubuntu 20.04 currently) you can simply use stack build and then copy the executable along with the neural net to an aws ec2 instance and run it.

After running stack build the executable will be in the following location:
```
./stack-work/install/x86_64-linux-tinfo6/<hash>/8.6.5/bin/
```

Simply copy this along with the net which is located at:
```
./data/mnistNetSigmoid.txt
```

Put them on the ec2 instance. Make sure the net is as follows from the path relative to the executable on the ec2 instance: ./data/mnistNetSigmoid.txt.

To run the exe liblas-dev and liblapack-dev need to be installed on the ec2 ubuntu:
```shell
sudo apt-get install -y libblas-dev liblapack-dev
```

Then running ./haskell-net-exe will launch the api on port 8081. Do this in a tmux session that can be detached. Use nginx as a proxy.