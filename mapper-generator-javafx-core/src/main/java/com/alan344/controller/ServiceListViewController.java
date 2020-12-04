package com.alan344.controller;

import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.init.ServiceListViewInit;
import com.alan344.service.node.NodeHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2020/9/9 9:58
 */
@Controller
public class ServiceListViewController implements Initializable {

    @FXML
    private BorderPane borderPane;

    @FXML
    private ListView<VBox> listView;

    @Resource
    private ServiceListViewInit serviceListViewInit;

    @Resource
    private NodeHandler nodeHandler;

    @Resource
    private BeanFactory beanFactory;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        serviceListViewInit.setListView(BaseConstants.selectedTableNameTableMap.values(), listView);
        nodeHandler.addNode(borderPane);
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    @FXML
    public void next() {
        Node next = nodeHandler.getNext();
        if (next == null) {
            next = FxmlLoadFactory.create("/fxml/service-setup.fxml", beanFactory);
        }

        NodeConstants.borderPaneWrap.setCenter(next);
    }
}
