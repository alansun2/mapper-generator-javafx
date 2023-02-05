package com.alan344.bean.config;

import com.alan344.componet.LeftRightLinkageBorderPane;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2022/11/21 14:32
 */
@Getter
@Setter
public class ExtraFileGroupConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;
    /**
     * 是否开启
     */
    private boolean enable;
    /**
     * 是否是系统内置的配置
     */
    private boolean isSystem;
    /**
     * 额外文件的名称
     */
    private Set<ExtraFileConfig> extraFileConfigs;

    @Override
    public Collection<ExtraFileConfig> getList() {
        return this.extraFileConfigs;
    }

    @Override
    public void setList(Collection list) {
        this.extraFileConfigs = new HashSet<>(list);
    }

    @Override
    public ExtraFileGroupConfig clone() {
        try {
            ExtraFileGroupConfig clone = (ExtraFileGroupConfig) super.clone();
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            clone.setExtraFileConfigs(extraFileConfigs.stream().map(ExtraFileConfig::clone).collect(Collectors.toSet()));
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    @Getter
    @Setter
    public static class ExtraFileConfig implements Cloneable {
        private String templateId;
        private String name;
        /**
         * 文件输出地址
         */
        private String outputPath;
        /**
         * 包名
         */
        private String packageName;
        /**
         * 是否开启
         */
        private boolean enable;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            ExtraFileConfig that = (ExtraFileConfig) o;

            return templateId.equals(that.templateId);
        }

        @Override
        public int hashCode() {
            return templateId.hashCode();
        }

        @Override
        public ExtraFileConfig clone() {
            try {
                ExtraFileConfig clone = (ExtraFileConfig) super.clone();
                // TODO: copy mutable state here, so the clone can't change the internals of the original
                return clone;
            } catch (CloneNotSupportedException e) {
                throw new AssertionError();
            }
        }
    }
}
