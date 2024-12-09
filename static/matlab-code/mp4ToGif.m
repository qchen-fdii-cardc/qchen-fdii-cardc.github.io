[filename1, pathname1] = uigetfile({'*.mp4';'*.avi';'*.*'},'File Selector'); % 获取MP4或者Avi格式的视频文件路径以及名称
video=VideoReader([pathname1,filename1]); % 读取视频


numFrames = video.NumFrames; % 获取视频帧的总数
Framerate=video.FrameRate; % 获取视频帧率


bar = waitbar(0,'Please wait...'); % 设置进度条

start_time = 0; % 开始时间
% end_time = 100; % 结束时间

% if end_time>video.Duration
end_time=video.Duration;
% end

start_frame=round(Framerate*start_time)+1; % 开始帧数
end_frame=round(Framerate*end_time)-1; % 结束帧数

for i=start_frame:2:end_frame
    frame=read(video,i); %读取第k帧画面
    frame=imresize(frame,0.5); % 将图像缩小0.5倍
    % im=frame2im(frame); % 从单个影片帧frame返回真彩色 (RGB) 图像
    % 制作gif文件，图像必须是index索引图像,只能用256色
    [I,map]=rgb2ind(frame,256); % 将RGB图像frame转换为索引图像I，map为关联颜色图
    % I=rgb2gray(frame);
    waitbar((i-start_frame)/(end_frame-start_frame),bar,['转换中',num2str(round((i-start_frame)*100/(end_frame-start_frame))),'%']);
    fn = strcat(pathname1,filename1(1:end-4),'0.gif');
    delayTime = 0.067;
    if i==start_frame
        % 第一张直接保存到视频目录下
        imwrite(I,map,fn,'gif','Loopcount', inf, 'DelayTime', delayTime);
    else
        % 剩下的每张图续接上一个图，每张图间隔为与视频中的一致（0.067s，帧率为30）
        imwrite(I,map,fn,'gif','WriteMode', 'Append', 'DelayTime', delayTime);
    end
end

waitbar(1,bar,'finished');
close(bar);
