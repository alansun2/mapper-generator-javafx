package com.alan344.controller;

import com.alan344.bean.Column;
import com.alan344.bean.ColumnOverride;
import com.alan344.service.ColumnService;
import com.alan344happyframework.util.StringUtils;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.scene.image.Image;
import javafx.stage.Modality;
import javafx.stage.Stage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/19 18:53
 */
@Slf4j
@Controller
public class ColumnUpdateController implements Initializable {
    @FXML
    private TextField columnNameText;
    @FXML
    private TextField propertyText;
    @FXML
    private TextField javaTypeText;
    @FXML
    private TextField typeHandlerText;
    @FXML
    private CheckBox isGeneratedAlways;
    @FXML
    private CheckBox delimitedColumnName;

    @Autowired
    private ColumnService columnService;

    @Autowired
    private BeanFactory beanFactory;

    private Stage columnUpdateStage;

    private Column selectedColumn;

    @Override
    public void initialize(URL location, ResourceBundle resources) {

    }

    /**
     * 打开字段修改页面
     */
    void openStage(Stage primaryStage, Column column) {
        if (columnUpdateStage == null) {
            FXMLLoader fxmlLoader = new FXMLLoader();
            fxmlLoader.setLocation(getClass().getResource("/fxml/column-update.fxml"));
            fxmlLoader.setControllerFactory(beanFactory::getBean);

            columnUpdateStage = new Stage();
            try {
                columnUpdateStage.setScene(new Scene(fxmlLoader.load()));
            } catch (IOException e) {
                log.error("error", e);
                return;
            }
            columnUpdateStage.setResizable(false);
            columnUpdateStage.getIcons().add(new Image("/image/column@32.png"));
            columnUpdateStage.setTitle("column override");
            columnUpdateStage.initOwner(primaryStage);
            columnUpdateStage.initModality(Modality.WINDOW_MODAL);
        }

        selectedColumn = column;

        columnNameText.setText(column.getColumnName());
        ColumnOverride columnOverride = column.getColumnOverride();
        if (columnOverride != null) {
            propertyText.setText(StringUtils.getDefaultIfNull(columnOverride.getProperty(), null));
            javaTypeText.setText(StringUtils.getDefaultIfNull(columnOverride.getJavaType(), null));
            typeHandlerText.setText(StringUtils.getDefaultIfNull(columnOverride.getTypeHandler(), null));
            isGeneratedAlways.setSelected(columnOverride.isGeneratedAlways());
            delimitedColumnName.setSelected(columnOverride.isDelimitedColumnName());
        } else {
            propertyText.setText(null);
            javaTypeText.setText(null);
            typeHandlerText.setText(null);
            isGeneratedAlways.setSelected(false);
            delimitedColumnName.setSelected(false);
        }

        columnUpdateStage.show();
    }

    @FXML
    public void cancel() {
        columnUpdateStage.close();
    }

    @FXML
    public void apply() {
        ColumnOverride columnOverride = null;
        if (StringUtils.isNotEmpty(propertyText.getText())) {
            columnOverride = getColumnOverride(null);
            columnOverride.setProperty(propertyText.getText());
        }

        if (StringUtils.isNotEmpty(javaTypeText.getText())) {
            columnOverride = getColumnOverride(columnOverride);
            columnOverride.setJavaType(javaTypeText.getText());
        }

        if (StringUtils.isNotEmpty(typeHandlerText.getText())) {
            columnOverride = getColumnOverride(columnOverride);
            columnOverride.setJavaType(typeHandlerText.getText());
        }

        if (isGeneratedAlways.isSelected()) {
            columnOverride = getColumnOverride(columnOverride);
            columnOverride.setGeneratedAlways(true);
        }

        if (delimitedColumnName.isSelected()) {
            columnOverride = getColumnOverride(columnOverride);
            columnOverride.setGeneratedAlways(true);
        }

        if (columnOverride != null) {
            selectedColumn.setColumnOverride(columnOverride);
        }
    }

    private ColumnOverride getColumnOverride(ColumnOverride columnOverride) {
        if (columnOverride == null) {
            columnOverride = new ColumnOverride();
        }
        return columnOverride;
    }
}
