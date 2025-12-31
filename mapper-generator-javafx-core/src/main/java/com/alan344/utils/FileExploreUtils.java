package com.alan344.utils;

import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.IOException;

/**
 * @author AlanSun
 * @since 2020/7/1 9:38
 */
@Slf4j
public class FileExploreUtils {
    /**
     * 打开输出目录
     */
    public static void open(String outDir) {
        log.info("打开输出目录: {}", outDir);
        if (StringUtils.isNotEmpty(outDir)) {
            File file = new File(outDir);
            if (!file.exists()) {
                log.error("文件不存在: {}", outDir);
                throw new RuntimeException("文件不存在");
            }
            if (!file.isDirectory()) {
                file = file.getParentFile();
            }
            try {
                String osName = System.getProperty("os.name");
                if (osName != null) {
                    if (osName.contains("Mac")) {
                        Runtime.getRuntime().exec("open " + file.getAbsolutePath());
                    } else if (osName.contains("Windows")) {
                        Runtime.getRuntime().exec("cmd /c start " + file.getAbsolutePath());
                    } else {
                        log.error("不支持的操作系统: {}", osName);
                        throw new RuntimeException("不支持的操作系统");
                    }
                }
            } catch (IOException e) {
                log.error("打开文件目录失败", e);
                throw new RuntimeException("打开文件目录失败");
            }
        }
    }
}
