package com.alan344.controller;

import com.alan344.constants.NodeConstants;
import com.alan344.controller.component.MybaitsExportController;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.TextUtils;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import org.mybatis.generator.my.config.ServiceConfig;
import org.mybatis.generator.my.config.ServiceConfigThreadLocal;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/9/10 10:10
 */
@Controller
public class ServiceSetupController implements Initializable {

    @FXML
    private BorderPane borderPane;

    @FXML
    private VBox vBox;
    @FXML
    private TextField configNameText;
    @FXML
    private TextField requestPackageText;
    @FXML
    private TextField requestIgnoreColumnText;
    @FXML
    private TextField servicePackageText;
    @FXML
    private TextField controllerPackageText;

    @Resource
    private MybaitsExportController mybaitsExportController;

    @Resource
    private ExportService exportService;

    @Resource
    private NodeHandler nodeHandler;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        nodeHandler.addNode(borderPane);

        // 绑定文本框和tab的宽度
        final ObservableList<Node> hBoxs = vBox.getChildren();
        hBoxs.forEach(node -> {
            final HBox hBox = (HBox) node;
            final ObservableList<Node> children = hBox.getChildren();
            if (children.size() > 1) {
                final Node node1 = children.get(1);
                if (node1 instanceof TextField) {
                    ((TextField) node1).prefWidthProperty().bind(vBox.widthProperty().multiply(0.6));
                }
            } else if (children.size() == 1) {
                final Node node1 = children.get(0);
                if (node1 instanceof TabPane) {
                    ((TabPane) node1).prefWidthProperty().bind(vBox.widthProperty());
                }
            }
        });
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    @FXML
    public void export() {
        this.validExport();
        final ServiceConfig serviceConfig = new ServiceConfig();
        serviceConfig.setConfigName(configNameText.getText());
        serviceConfig.setRequestPackage(requestPackageText.getText());
        serviceConfig.setServicePackage(servicePackageText.getText());
        serviceConfig.setControllerPackage(controllerPackageText.getText());
        serviceConfig.setRequestGlobalIgnoreColumns(requestIgnoreColumnText.getText());
        ServiceConfigThreadLocal.setServiceConfig(serviceConfig);
        exportService.export();
    }

    @FXML
    public void expand() {

    }

    /**
     * 校验配置是否符合要求
     */
    public void validExport() {
        mybaitsExportController.validExport();
        TextUtils.checkTextsHasEmpty(NodeConstants.primaryStage, configNameText);
    }
}
