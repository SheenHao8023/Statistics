import turtle

screen = turtle.Screen()
screen.bgcolor("white")
pen = turtle.Turtle()
pen.speed(0)  # 设置画笔速度
pen.width(2)  # 设置画笔宽度为2

def draw_circle(radius):  # 定义绘制单个圆的函数
    pen.color("gray")
    pen.penup()  # 抬起画笔
    pen.goto(0, -radius)  # 移动到圆的底部中心点
    pen.pendown()  # 放下画笔
    pen.circle(radius)
def draw_arc(radius, angle, color): # 定义绘制弧线的函数
    pen.color(color)
    pen.penup()
    pen.goto(0, -radius)
    pen.pendown()
    pen.circle(radius, angle)

inner_r = 50  # 最内圈圆的半径
distance = 30  # 同心圆之间的距离

for _ in range(8):
    draw_circle(inner_r)
    inner_r += distance
angles = [119.8485291,119.6215227,110.3535899,114.4678818,
          106.6020483,97.46297979,121.0351807,120.4199648,
          126.738442,128.7014431,121.4277557,119.3288587,
          121.8449527,125.6344479,131.0397719,127.0563395]

# 在相应的同心圆上绘制弧线
for i, angle in enumerate(angles):
    circle_index = i // 2  # 确定圆的索引
    r = inner_r + circle_index * distance
    if i % 2 == 0:  # 偶数索引绘制红色弧线
        color = "red"
        draw_arc(r, angle, color) # 
    else:  # 奇数索引绘制绿色弧线
        color = "green"
        draw_arc(r, -angle, color) # 

pen.hideturtle() # 隐藏画笔
screen.mainloop() # 保持窗口打开直到用户关闭
