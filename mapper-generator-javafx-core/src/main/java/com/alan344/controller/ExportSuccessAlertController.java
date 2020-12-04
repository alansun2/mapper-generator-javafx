package com.alan344.controller;

import org.mybatis.generator.my.config.MybatisExportConfig;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.utils.FileUtils;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.paint.Paint;
import javafx.scene.text.Text;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;

/**
 * @author AlanSun
 * @date 2020/4/6 18:29
 */
@Slf4j
@Controller
public class ExportSuccessAlertController {

    @FXML
    private Text text;

    @FXML
    private ImageView image;

    @FXML
    private Button openFileButton;

    @Resource
    private BeanFactory beanFactory;

    private Stage tableAdvanceSetUpStage;

    private MybatisExportConfig mybatisExportConfig;

    /**
     * 成功导出后的 button 按钮显示文字
     */
    private static final String OPEN_FILE_TEXT_SUCCESS = "打开文件夹";

    /**
     * 打开表高级设置
     *
     * @param stage 主窗口
     */
    public void openTableAdvancedSetup(Stage stage, boolean isExportSuccess, MybatisExportConfig mybatisExportConfig) {
        if (tableAdvanceSetUpStage == null) {
            AnchorPane tableAdvanceSetUpPane = FxmlLoadFactory.create("/fxml/component/export-alert.fxml", beanFactory);
            tableAdvanceSetUpStage = new Stage();
            tableAdvanceSetUpStage.setScene(new Scene(tableAdvanceSetUpPane));
            tableAdvanceSetUpStage.setTitle("导出成功");
            tableAdvanceSetUpStage.getIcons().add(new Image("/image/advanced-set-up.png"));
            tableAdvanceSetUpStage.setResizable(false);
            tableAdvanceSetUpStage.initModality(Modality.WINDOW_MODAL);
            tableAdvanceSetUpStage.initOwner(stage);
        }

        this.mybatisExportConfig = mybatisExportConfig;

        if (isExportSuccess) {
            tableAdvanceSetUpStage.setTitle("导出成功");
            text.setText("successful!!!");
            text.setFill(Paint.valueOf("#25ae20"));
            text.setWrappingWidth(180);
            image.setImage(new Image("/image/export-success.png"));
            openFileButton.setText(OPEN_FILE_TEXT_SUCCESS);
        } else {
            tableAdvanceSetUpStage.setTitle("导出失败");
            text.setText("error，请查看日志文件");
            text.setFill(Paint.valueOf("#AE4F13"));
            text.setLayoutX(75);
            text.setWrappingWidth(220);
            image.setImage(new Image("/image/export-error.png"));
            image.setLayoutX(30);
            openFileButton.setVisible(false);
        }

        tableAdvanceSetUpStage.show();
    }

    /**
     * 关闭
     */
    @FXML
    public void close() {
        tableAdvanceSetUpStage.close();
    }

    @FXML
    public void openFileAfterExport() {
        if (OPEN_FILE_TEXT_SUCCESS.equals(openFileButton.getText())) {
            FileUtils.open(mybatisExportConfig.getBeanLocation());
        }

        this.close();
    }
}
