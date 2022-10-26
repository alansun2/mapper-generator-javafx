package com.alan344.utils;

import lombok.extern.slf4j.Slf4j;
import com.alan344.utils.StringUtils;

import java.io.IOException;

/**
 * @author AlanSun
 * @date 2020/7/1 9:38
 */
@Slf4j
public class FileUtils {
    /**
     * 打开输出目录
     */
    public static void open(String outDir) {
        if (StringUtils.isNotEmpty(outDir)) {
            try {
                String osName = System.getProperty("os.name");
                if (osName != null) {
                    if (osName.contains("Mac")) {
                        Runtime.getRuntime().exec("open " + outDir);
                    } else if (osName.contains("Windows")) {
                        Runtime.getRuntime().exec("cmd /c start " + outDir);
                    } else {
                        log.debug("文件输出目录:" + outDir);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        open("D:\\software\\navicat-for-mysql");
    }
}
