ssh-keygen -t rsa -C "xxx@gmail.com" -->  生成密钥对

git config --global user.name "xxx"  |git config --global user.email xxx@gmail.com --> Git全局用户设置

git init --> 进行初始化工作，在当前目录下生成 .git目录，其中包含Git所需要的所有数据和资源

git clone URL --> 从指定地址copy项目到本地。

git config --list --> 查看配置信息

git status --> 查看当前状态

git add . --> 添加当前代码

git commit --> 将代码提交到本地

git reset [filename] --> 撤销commit 的名为filename的文件，不写则撤销上次的全部操作

git branch --> 查看本地分支 (本地默认分支为 master)

git remote --> 查看远程分支 (远程默认分支为 origin)

git branch branch_name --> 添加分支 branch_name

git remote add origin_name  git@github.com:xxx/new-project.git --> 添加分支origin_name

git checkout origin/branch_name1  -b branch_name2 --> 将远程分支checkout到本地，并且创建名为 branch_name2 的分支

git checkout [name] --> 切换到name这个branch上

git push origin [name] --> 将name分支下的代码同步到github上。

git push == git push origin master -->  同上

git log --> 版本提交信息

git diff 版本号0 版本号1 --> 比较版本号0和版本号1的不同，显示，并标记出有代码改动的地方。

补充一个可视化比较的工具：meld

sudo apt-get install meld --> 安装meld

git config --global diff.tool meld --> 在全局配置上meld工具 ,然后，git difftool 就相当于meld命令。

git config --global difftool.prompt false --> 把提示设置为false

git config --global alias.df difftool --> 将difftool命令用df代替。(起一个别名，用于简化命令)


git format-patch -1 [branch-name] --> 在本分支下生成patch
