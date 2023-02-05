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
public class ExtraFileGroupConfig implements LeftRightLinkageBorderPane.GroupName<ExtraFileGroupConfig.ExtraFileConfig>, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;
    /**
     * 是否是系统内置的配置
     */
    private Boolean isSystem;
    /**
     * 是否开启
     */
    private boolean enable;
    /**
     * 额外文件的名称
     */
    private Set<ExtraFileConfig> extraFileConfigNames;

    @Override
    public Collection<ExtraFileConfig> getList() {
        return this.extraFileConfigNames;
    }

    @Override
    public void setList(Collection<ExtraFileConfig> list) {
        this.extraFileConfigNames = new HashSet<>(list);
    }

    @Override
    public ExtraFileGroupConfig clone() {
        try {
            ExtraFileGroupConfig clone = (ExtraFileGroupConfig) super.clone();
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            clone.setExtraFileConfigNames(extraFileConfigNames.stream().map(ExtraFileConfig::clone).collect(Collectors.toSet()));
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    @Getter
    @Setter
    public static class ExtraFileConfig implements Cloneable{
        private String groupName;
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

            return name.equals(that.name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
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
