# Config steps.

1. Sign up for GitHub if you dont have an acount.
2. Get a ssh key on the terminal of linux:
> ssh -keygen -t -rsa -C "your_email@youremail.com"

This key will be saved in a file .ssh/id_rsa.pub of your home dir.
3. Back to github, add the ssh key to account settings-SSH Keys. Title is for you to distingush different local computer.
4. Test whether the SSH key was successful added:
> ssh -T git@github.com

If success when "You’ve successfully authenticated, but GitHub does not provide shell access" appeared.
If not when "gent admitted failure to sign using the key.Permission denied (publickey).", try:
> ssh -add 
5. config with:
git config --global user.name "your name"
git config --global user.email "your email"
When success config, there is a file called ".gitconfig" in home dir, it record your github information.

A Chinese version of this steps see [here](http://blog.csdn.net/u012336923/article/details/44194457).
