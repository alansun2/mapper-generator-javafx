package com.alan344.factory;

import javafx.stage.DirectoryChooser;
import javafx.stage.Stage;
import com.alan344.utils.StringUtils;

import java.io.File;

/**
 * @author AlanSun
 * @date 2019/8/16 9:49
 */
public class FileDirChooserFactory {

    /**
     * 文件夹选择器
     */
    public static File createDirectoryScan(String title, String initDirectory) {
        DirectoryChooser directoryChooser = new DirectoryChooser();
        directoryChooser.setTitle(title);
        if (StringUtils.isNotEmpty(initDirectory)) {
            directoryChooser.setInitialDirectory(new File(initDirectory));
        }
        Stage fileStage = new Stage();
        return directoryChooser.showDialog(fileStage);
    }
}
