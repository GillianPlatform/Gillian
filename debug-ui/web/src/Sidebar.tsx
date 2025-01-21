import { VscSparkle, VscTarget } from "react-icons/vsc";
import React, { ReactNode } from "react";
import styled from "styled-components";
import IconButton from "./IconButton";

const Wrap = styled.div`
  flex-grow: 1;
  padding: 19px;
  --border: 1px solid var(--vscode-editorGroup-border);
  --table-border: 1px solid var(--vscode-editorRuler-foreground);
  border-left: var(--border);
  @media (max-width: 500px) {
    border-left: none;
    border-top: var(--border);
  }

  & table {
    font-family: var(--vscode-editor-font-family);
    border-collapse: collapse;
  }

  & tr {
    border-top: var(--table-border);
  }

  & tr:first-child {
    border-top: none;
  }

  & td {
    padding: 0 0.25em;
  }

  & td:first-child {
    border-right: var(--table-border);
  }
`;

export type Subst = readonly [string, string, string];

export type SidebarProps = {
  substs?: Subst[] | undefined;
  selectedNodes: readonly string[];
  onNodeSelected: (id: string) => void;
};

const Sidebar: React.FC<SidebarProps> = ({ substs, selectedNodes, onNodeSelected }) => {
  if (!substs) {
    return <></>;
  }
  let content: ReactNode;
  if (substs.length === 0) {
    content = <i>No substitutions.</i>;
  } else {
    content = (
      <table>
        <tbody>
          {substs.map(([id, from, to], i) => (
            <tr key={i}>
              <td>
                {selectedNodes.includes(id) ? (
                  <IconButton disabled>
                    <VscSparkle />
                  </IconButton>
                ) : (
                  <IconButton onClick={() => onNodeSelected(id)}>
                    <VscTarget />
                  </IconButton>
                )}
              </td>
              <td>{from}</td>
              <td>→</td>
              <td>{to}</td>
            </tr>
          ))}
        </tbody>
      </table>
    );
  }
  return (
    <Wrap>
      <h2>Substitutions</h2>
      {content}
    </Wrap>
  );
};

export default Sidebar;
