package com.alan344.factory;

import com.alan344.utils.StringUtils;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;

/**
 * @author AlanSun
 * @since 2019/8/16 9:49
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

    /**
     * 文件选择器
     */
    public static File createFileScan(String title, String initDirectory, String filterDesc, String... extensions) {
        if (StringUtils.contains(initDirectory, "classpath")) {
            initDirectory = "";
        }
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle(title);
        fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(filterDesc, extensions));
        if (StringUtils.isNotEmpty(initDirectory)) {
            fileChooser.setInitialDirectory(new File(initDirectory));
        }
        Stage fileStage = new Stage();
        return fileChooser.showOpenDialog(fileStage);
    }
}
