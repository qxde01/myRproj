MVC
============
> 
用R实现MVC(Mean-value Coordinates)图像编辑算法，Possion图像编辑的近似算法，论文见[Coordinates for Instant Image Cloning](www.cs.huji.ac.il/~danix/mvclone/files/mvc-final-opt.pdf)。这里并没有要求严格的轮廓边界点，仅用矩形框代替轮廓。在MVC的R代码中，异常值没有处理好，造成有斑点。
>
MVC算法要求背景不能太复杂，否则效果太差，如果前景背景相近，效果较好。图像编辑主要通过手动三分图实现，比如Photoshop，自动实现三分图还是比较困难的，效果还不一定好。