#Contributor
A contributor is someone from outside not on the core development team of the project taht wants to contribute some changes to a project. There are three level of code for these work:
1. upstream, 权限最高的、代码原作者。
2. oringin, fork的代码在你github上的库，代表远程库
3. local, 从oringin上clone下来存在的库 

> You first fork a project of some by click the fork on tht top right of github on the project page.

> Then clone it to your (local) computer or server by :

git clone <repository_site>

> Do some change to your local, commit and push it to your github(origin)

git add change_file
git commit -m "some description about your change"
git push origin master:master

> After that, a change will be apear on your github web, as well as a pull request.

Click the pull request button and make some description, the author of the source project(upstream) will receive a message about your change. If he/her think your change is acceptable, your change will be a part of his work, and you will be a contributor. The github will show your name in his project.

# Cooperator
Someone can be a cooperator by setting function of github, when you become a contributor of a project, you have the super right of a project including push and fetch.

> master branch, 主分支，是github在创建库的时候默认的分支，即master。这个分支用来合并版本，我的理解是，合并重要的，全面的工作。
Do not change master branch frequently. It is just changed when a version released.

> We can have a branch to do daily modification, others can not see it before you push it to github.
我们可以构建其他的分支如dev进行日常的修改和创作。
每个人可以在并在需要的时候，将自己的工作push到dev branch.

> When a file commit by more than one authors, both commit will apear a conflic.

Now, you should do :
git pull origin dev:dev 
to make your local up to date, and change the file manual delete >>> and === and <<<< auto added by git. And give a push so that the change of your both will be resonable.

With a detailed description of github branched and cooperation, see here(https://github.com/structureddynamics/OSF-Web-Services/wiki/Collaboration:-Git-Development-Workflow) and 廖雪峰博客(https://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000/001375840038939c291467cc7c747b1810aab2fb8863508000)

