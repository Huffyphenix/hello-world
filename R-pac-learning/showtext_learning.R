##########################################
# use showtext package to change font face in R graphs
##########################################

install.packages("showtext")
library(showtext)

# �鿴��ǰϵͳ����洢λ��
font.paths()
# list available font files in the search path.
font_files()

# basic usage -------------------------------------------------------------

# ���뿬��
font_add("kaishu", "simkai.ttf");

font.families()
install.packages("Cairo") # Cairo, a pac produce high-quality output.
library(Cairo);
# ��ͼ���豸
CairoPNG("chinese-char-noshowtext.png", 600, 600);
# ��ʼʹ��showtext
showtext.begin();
# һϵ�л�ͼ����
set.seed(123);
plot(1, xlim = c(-3, 3), ylim = c(-3, 3), type = "n");
text(runif(100, -3, 3), runif(100, -3, 3),
     intToUtf8(round(runif(100, 19968, 40869)), multiple = TRUE),
     col = rgb(runif(100), runif(100), runif(100), 0.5 + runif(100)/2),
     cex = 2, family = "kaishu");    # ָ��kaishu����
title("�������", family = "wqy-microhei");   # ָ��wqy����
# ֹͣʹ��showtext
showtext.end();
# �ر�ͼ���豸
dev.off();

# usage 1 -----------------------------------------------------------------
# download font from internet
link = "http://img.dafont.com/dl/?f=wm_people_1";
download.file(link, "wmpeople1.zip", mode = "wb"); # donwload a font type to local PC
unzip("wmpeople1.zip"); # decompress it

font.add("wmpeople1", "wmpeople1.TTF"); # add font type from local PC location
library(ggplot2);
library(plyr);
library(Cairo);

dat = read.csv(textConnection('edu,educode,gender,population
                              δ�Ϲ�ѧ,1,m,17464
                              δ�Ϲ�ѧ,1,f,41268
                              С  ѧ,2,m,139378
                              С  ѧ,2,f,154854
                              ��  ��,3,m,236369
                              ��  ��,3,f,205537
                              ��  ��,4,m,94528
                              ��  ��,4,f,70521
                              ��ר������,5,m,57013
                              ��ר������,5,f,50334'
                              ));
dat$int = round(dat$population / 10000);
gdat = ddply(dat, "educode", function(d) {
  male = d$int[d$gender == "m"];
  female = d$int[d$gender == "f"];
  data.frame(gender = c(rep("m", male), rep("f", female)),
             x = 1:(male + female));
});
gdat$char = ifelse(gdat$gender == "m", "p", "u");

CairoPNG("edu-stat.png", 600, 300);
pdf("edu-stat.pdf", 6, 3)
showtext.begin();
theme_set(theme_grey(base_size = 15));
ggplot(gdat, aes(x = x, y = educode)) +
  geom_text(aes(label = char, colour = gender),
            family = "wmpeople1", size = 8) +
  scale_x_continuous("������ǧ��") +
  scale_y_discrete("�ܽ����̶�",
                   labels = unique(dat$edu[order(dat$educode)])) +
  scale_colour_hue(guide = FALSE) +
  ggtitle("2012���˿�ͳ������");
showtext.end();
dev.off();
# usage 2 -----------------------------------------------------------------

link = "http://img.dafont.com/dl/?f=emoticons";
download.file(link, "emoticons.zip", mode = "wb");
unzip("emoticons.zip");

library(showtext);
font.add("emoticons", "emoticons.ttf");

library(ggplot2);
library(Cairo);
emotions = c("W", "s", "C", "A", "p");
score = c(0.5, 0.9, 5.5, 18.4, 74.7);
x = factor(emotions, emotions);
gdat2 = data.frame(x, score);
CairoPNG("douban.png", 600, 600);
showtext.begin();
ggplot(gdat2, aes(x = x, y = score)) +
  geom_bar(stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("�ٷֱ�") +
  theme(axis.text.x=element_text(size=rel(4), family="emoticons")) +
  ggtitle("����̽����˵���������������");
showtext.end();
dev.off();


# usage -------------------------------------------------------------------

font.add("fang", "simfang.ttf") ## add font
pdf("showtext-ex1.pdf")
plot(1, type = "n")
showtext.begin()                ## turn on showtext
text(1, 1, intToUtf8(c(82, 35821, 35328)), cex = 10, family = "fang") 
#The use of intToUtf8() is for convenience if you can��t view or input Chinese characters. You can instead use
text(1, 1, "R����", cex = 10, family = "fang")
showtext.end()                  ## turn off showtext
dev.off()

# usage -------------------------------------------------------------------
library(showtext)

wd = setwd(tempdir())
download.file("http://fontpro.com/download-family.php?file=35701",
              "merienda-r.ttf", mode="wb")
download.file("http://fontpro.com/download-family.php?file=35700",
              "merienda-b.ttf", mode="wb")
font.add("merienda",
         regular = "merienda-r.ttf",
         bold = "merienda-b.ttf")
setwd(wd)

pdf("showtext-ex2.pdf", 7, 4)
plot(1, type = "n", xlab = "", ylab = "")
showtext.begin()
par(family = "merienda")
text(1, 1.2, "R can use this font!", cex = 2)
text(1, 0.8, "And in Bold font face!", font = 2, cex = 2)
showtext.end()
dev.off()
