unit Entities.Bird;

interface

uses
  Winapi.OpenGL, System.Math, Core.Interfaces, Core.Types;

type
  TBird = class(TInterfacedObject, IGameObject, IUpdatable, IRenderable)
  private
    FPosition: TVector3;
    FAngle: Single;
    FWingAngle: Single;
    FTime: Double;
    
    // Geometry data
    FBodyVertices: array of TVector3;
    FLeftWingVertices: array of TVector3;
    FRightWingVertices: array of TVector3;
    
    procedure InitGeometry;
    procedure DrawBody;
    procedure DrawWings;
  public
    constructor Create;
    
    { IGameObject }
    function GetUpdatable: IUpdatable;
    function GetRenderable: IRenderable;
    
    { IUpdatable }
    procedure Update(const DeltaTime: Double);
    
    { IRenderable }
    procedure Render;
  end;

implementation

{ TBird }

constructor TBird.Create;
begin
  FPosition := TVector3.Create(0, 0, 0);
  FAngle := 0;
  FWingAngle := 0;
  FTime := 0;
  InitGeometry;
end;

procedure TBird.InitGeometry;
begin
  // Simple bird body (pyramid-like shape)
  SetLength(FBodyVertices, 12); // 4 triangles * 3 vertices
  
  // Top
  FBodyVertices[0] := TVector3.Create(0, 0.5, 0);
  FBodyVertices[1] := TVector3.Create(-0.5, -0.5, 0.5);
  FBodyVertices[2] := TVector3.Create(0.5, -0.5, 0.5);
  
  // Bottom
  FBodyVertices[3] := TVector3.Create(0, 0.5, 0);
  FBodyVertices[4] := TVector3.Create(0.5, -0.5, 0.5);
  FBodyVertices[5] := TVector3.Create(0.5, -0.5, -0.5);
  
  // Back
  FBodyVertices[6] := TVector3.Create(0, 0.5, 0);
  FBodyVertices[7] := TVector3.Create(0.5, -0.5, -0.5);
  FBodyVertices[8] := TVector3.Create(-0.5, -0.5, -0.5);
  
  // Left
  FBodyVertices[9] := TVector3.Create(0, 0.5, 0);
  FBodyVertices[10] := TVector3.Create(-0.5, -0.5, -0.5);
  FBodyVertices[11] := TVector3.Create(-0.5, -0.5, 0.5);

  // Wings (simple triangles)
  SetLength(FLeftWingVertices, 3);
  FLeftWingVertices[0] := TVector3.Create(0, 0, 0);      // Shoulder
  FLeftWingVertices[1] := TVector3.Create(-1.5, 0, 0.5); // Tip
  FLeftWingVertices[2] := TVector3.Create(-0.5, 0, -0.5); // Back

  SetLength(FRightWingVertices, 3);
  FRightWingVertices[0] := TVector3.Create(0, 0, 0);     // Shoulder
  FRightWingVertices[1] := TVector3.Create(1.5, 0, 0.5); // Tip
  FRightWingVertices[2] := TVector3.Create(0.5, 0, -0.5); // Back
end;

function TBird.GetUpdatable: IUpdatable;
begin
  Result := Self;
end;

function TBird.GetRenderable: IRenderable;
begin
  Result := Self;
end;

procedure TBird.Update(const DeltaTime: Double);
begin
  FTime := FTime + DeltaTime;
  
  // Bobbing motion (up and down)
  FPosition.Y := Sin(FTime * 2.0) * 0.5;
  
  // Flapping motion (wings rotation)
  FWingAngle := Sin(FTime * 10.0) * 30.0; // Fast flapping
  
  // Rotation (spinning slowly)
  FAngle := FAngle + (DeltaTime * 20.0);
  if FAngle > 360 then FAngle := FAngle - 360;
end;

procedure TBird.DrawBody;
begin
  glColor3f(clBirdBody.R, clBirdBody.G, clBirdBody.B);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, @FBodyVertices[0]);
  glDrawArrays(GL_TRIANGLES, 0, Length(FBodyVertices));
  glDisableClientState(GL_VERTEX_ARRAY);
end;

procedure TBird.DrawWings;
begin
  glColor3f(clBirdWing.R, clBirdWing.G, clBirdWing.B);
  
  // Left Wing
  glPushMatrix;
  glTranslatef(-0.5, 0, 0); // Move to shoulder
  glRotatef(FWingAngle, 0, 0, 1); // Flap
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, @FLeftWingVertices[0]);
  glDrawArrays(GL_TRIANGLES, 0, Length(FLeftWingVertices));
  glDisableClientState(GL_VERTEX_ARRAY);
  glPopMatrix;
  
  // Right Wing
  glPushMatrix;
  glTranslatef(0.5, 0, 0); // Move to shoulder
  glRotatef(-FWingAngle, 0, 0, 1); // Flap (opposite direction)
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, @FRightWingVertices[0]);
  glDrawArrays(GL_TRIANGLES, 0, Length(FRightWingVertices));
  glDisableClientState(GL_VERTEX_ARRAY);
  glPopMatrix;
end;

procedure TBird.Render;
begin
  glPushMatrix;
  
  // Apply position and rotation
  glTranslatef(FPosition.X, FPosition.Y, FPosition.Z);
  glRotatef(FAngle, 0, 1, 0);
  
  DrawBody;
  DrawWings;
  
  glPopMatrix;
end;

end.
